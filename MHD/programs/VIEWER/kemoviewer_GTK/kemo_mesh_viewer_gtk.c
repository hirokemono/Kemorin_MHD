
/* kemo_mesh_viewer_gtk.c*/

#include "kemo_mesh_viewer_gtk.h"

#define NPIX_X  800
#define NPIX_Y  640

struct glut_menu_address  glut_menu_id_struct;
struct glut_menu_address *glut_menu_id;

static int winid, menu_win;
static char viewtype_title[80] = "3D-View";

static void make_1st_level_menu();

struct kemoviewer_type *single_kemoview;


/* subroutine for reading mesh */


static void enter_leave(int state){
	printf("enter/leave %d = %s\n", glutGetWindow(), state == GLUT_LEFT ? "left" : "entered");
	return;
}

static void link_glut_menu_address(){
	glut_menu_id = &glut_menu_id_struct;
	return;
}


/* draw object using GLUT */

static void draw_mesh_w_menu(){
	make_1st_level_menu();
	draw_mesh_keep_menu();
	return;
};

static void save_image_handler(){
	int id_image;
    struct kv_string *image_prefix = kemoview_alloc_kvstring();
    
	id_image = output_image_file_gtk(image_prefix);
	
	glutSetWindow(winid);
	draw_mesh_keep_menu();
	
	if(id_image == 0) return;
    kemoview_write_window_to_file(id_image, image_prefix);
    kemoview_free_kvstring(image_prefix);
	return;
};

static void load_texture_handler(){
	int id_image;
    struct kv_string *image_prefix;
	id_image = input_texture_file_gtk(image_prefix);
	
	if(id_image == SAVE_PNG || id_image == SAVE_BMP){
        image_prefix = kemoview_alloc_kvstring();
		kemoview_set_texture_to_PSF(id_image, image_prefix);
		kemoview_set_PSF_patch_color_mode(TEXTURED_SURFACE);
        kemoview_free_kvstring(image_prefix);
	};
	
	
	glutSetWindow(winid);
	draw_mesh_w_menu();
	return;
};

static void save_evolution_handler(){
	int id_image;
	int ist_udt, ied_udt, inc_udt;
	int iflag;
    struct kv_string *image_prefix = kemoview_alloc_kvstring();
	
	ist_udt = kemoview_get_PSF_full_path_file_prefix(image_prefix, &iflag);
	id_image = output_evolution_file_gtk(image_prefix, &ist_udt, &ied_udt, &inc_udt);
	if(id_image == 0) return;
	
	printf("header: %s\n", image_prefix->string);
	printf("steps: %d %d %d\n", ist_udt, ied_udt, inc_udt);
	write_evolution_views_glut(id_image, image_prefix, ist_udt, ied_udt, inc_udt);
    kemoview_free_kvstring(image_prefix);
	return;
};

void draw_rot_image_handler(int id_rot){
    int inc_deg = 2;
    struct kv_string *image_prefix = kemoview_init_kvstring_by_string("Kemoviewer");
    
    write_rotate_views_glut(NO_SAVE_FILE, image_prefix, id_rot, inc_deg);
    kemoview_free_kvstring(image_prefix);
};

void save_rot_image_handler_gtk(){
	int id_image;
	int idir_rot = 2;
	int inc_rot;
    struct kv_string *image_prefix = kemoview_alloc_kvstring();
	
	id_image = output_rotation_file_gtk(image_prefix, &idir_rot, &inc_rot);
	write_rotate_views_glut(id_image, image_prefix, idir_rot, inc_rot);
	if(id_image == 0) return;
	
    kemoview_free_kvstring(image_prefix);
};


/* ---------  Action for selected menu -----------   */ 

static void read_draw_kemoview_data_gtk(){
	read_kemoview_data_gtk();
	draw_mesh_w_menu();
	return;
};

static void main_menu_handler(int sel){
	if (sel == QUIT_SELECTED)   { 
		exit(EXIT_SUCCESS);
	} else if(sel == FILE_OPEN)  { 
		read_draw_kemoview_data_gtk();
	} else if(sel == ADD_PSF_COLOR)  {
		gtk_psf_colormap_menu(single_kemoview);
		draw_mesh_w_menu();
	} else if(sel == MESH_OFF){
		gtk_mesh_menu(single_kemoview);
		draw_mesh_w_menu();
	} else if(sel == ISET_FLINE_THICK){
		set_fline_thick_gtk();
	}
	else if(sel == SAVE_SNAPSHOT)  { save_image_handler(); }
	else if(sel == SAVE_EVOLUTION) { save_evolution_handler(); }
	else if(sel == SAVE_ROTATION) { save_rot_image_handler_gtk(); }
    else if(sel == SET_BACKGROUND) {
		gtk_BGcolorselect(single_kemoview);
		draw_mesh_keep_menu();
	};
    return;
};

static void viewtype_handler(int sel){
	set_viewtype_mode_glut(sel, viewtype_title);
	draw_mesh_w_menu();
	return;
}

static void set_current_psf_handler(int sel){
	kemoview_set_current_PSF(sel);
	draw_mesh_w_menu();
	return;
};


static void color_mode_handler(int sel){
	if (sel == GRAYSCALE) {
		kemoview_set_mesh_color_mode(GRAYSCALE);
	} else if( sel == RAINBOW_COLOR) {
		kemoview_set_mesh_color_mode(RAINBOW_COLOR);
	} else if( sel == SET_NUM_COLORS) {
		set_num_color_loop_gtk();
	};

	draw_mesh_w_menu();
	return;
}

static void object_property_handler(int sel){
	int toggle;
    toggle = kemoview_toggle_object_properties(sel);

	if( sel == SET_COAST_RADIUS) {
		gtk_main_menu(single_kemoview);
	} else if( sel == OUTPUT_V_MATRIX) {
		save_viewmatrix_file_gtk();
	} else if( sel == INPUT_V_MATRIX) {
		load_viewmatrix_file_gtk();
	};
	draw_mesh_w_menu();
	return;
}


static void dummy_handler(int sel){
	int itmp;
	itmp = sel;
	return;
}


/* 2nd level menues*/

static void make_2nd_level_view_menu(){
	glut_menu_id->viewtype_id = glutCreateMenu(viewtype_handler);
	glut_viewtype_menu();
	
	return;
};

/* 2nd level menues*/

static void make_2nd_level_image_menu(){
	int iflag_draw_m = kemoview_get_draw_mesh_flag();
	int iflag_draw_p = kemoview_get_PSF_draw_switch();
	int iflag_draw_f = kemoview_get_fline_switch();
	int iflag_axis =       kemoview_get_object_property_flags(AXIS_TOGGLE);
	int iflag_draw_coast = kemoview_get_object_property_flags(COASTLINE_SWITCH);
	int iflag_draw_sph =   kemoview_get_object_property_flags(SPHEREGRID_SWITCH);
	
	glut_menu_id->color_mode_menu = glutCreateMenu(color_mode_handler);
	glut_color_mode_menu_item();
	
	glut_menu_id->polygon_id_menu = glutCreateMenu(object_property_handler);
	
	glutAddMenuEntry("Set objects",SET_COAST_RADIUS);
	
	glut_drawing_select();

	glutAddMenuEntry("Output transfer matrices",OUTPUT_V_MATRIX);
	glutAddMenuEntry("Load transfer matrices", INPUT_V_MATRIX);
	
	glut_menu_id->draw_rot_image_menu = glutCreateMenu(draw_rot_image_handler);
	glutAddMenuEntry("x-axis",ROTATE_X);
	glutAddMenuEntry("y-axis",ROTATE_Y);
	glutAddMenuEntry("z-axis",ROTATE_Z);
	return;
};

/* Create 1st level menu() */

static void make_1st_level_menu(){
	GLint menu_id;
	
	int iflag_draw_m = kemoview_get_draw_mesh_flag();
	int iflag_draw_p = kemoview_get_PSF_draw_switch();
	int iflag_draw_f = kemoview_get_fline_switch();
	int iflag_any_objects_on = iflag_draw_p + iflag_draw_m + iflag_draw_f;
	
	int nload_psf = kemoview_get_PSF_num_loaded();
	
	glutSetWindow(menu_win);
	
	if (iflag_any_objects_on > 0) {
		make_2nd_level_view_menu();
	};
	
	
	
	if (iflag_any_objects_on > 0) {
		make_2nd_level_image_menu();
	};
	
	glut_menu_id->submenu_id = menu_init();
	
	menu_id = glutCreateMenu(main_menu_handler);
	
	glutAddMenuEntry("Open...",FILE_OPEN);
	if(iflag_any_objects_on > 0){
		/*glutAddSubMenu("View Modifier",glut_menu_id->submenu_id);*/
		glutAddSubMenu(viewtype_title,glut_menu_id->viewtype_id);
	}
	if( iflag_draw_p > 0){
		glutAddMenuEntry("PSF",  ADD_PSF_COLOR);
	};
	if( iflag_draw_f > 0){
		glutAddMenuEntry("Field lines",     ISET_FLINE_THICK);
	};
	
	
	if( iflag_draw_m > 0){
		glutAddMenuEntry("Mesh menu",    MESH_OFF);
	};
	
	if( nload_psf > 0) glutAddSubMenu("Surface rendering", glut_menu_id->psf_root_menu);
	if( iflag_draw_f > 0) glutAddSubMenu("Field Lines", glut_menu_id->fline_root_menu);
	
	
	if ( iflag_draw_m > 0) {
		glutAddSubMenu("Color mode",         glut_menu_id->color_mode_menu);
	};
	
	if (iflag_any_objects_on > 0) {
		/*printf("polygon_id_menu \n");*/
		glutAddSubMenu("Object propaties",   glut_menu_id->polygon_id_menu);
		glutAddMenuEntry("Save image", SAVE_SNAPSHOT);
	};
	
	if( iflag_draw_p+iflag_draw_f > 0){
		glutAddMenuEntry("Save evolution images", SAVE_EVOLUTION);
	};
	
	if (iflag_any_objects_on > 0) {
		glutAddMenuEntry("Save rotate images", SAVE_ROTATION);
		glutAddSubMenu("Rotate on Window",   glut_menu_id->draw_rot_image_menu);
	};
    glutAddMenuEntry("Preferences...",SET_BACKGROUND);
	
	glutAddMenuEntry("Quit",QUIT_SELECTED);
	glutAttachMenu(GLUT_LEFT_BUTTON);
	return;
};

/* Main routine for C */

void draw_mesh_kemo(int iflag_streo_shutter, int iflag_dmesh) {
	int narg_glut = 0;
	char **arg_glut;
	int iflag_core_profile = 1;
    GLboolean bStereo;
	/* Initialize arrays for viewer */
	
	single_kemoview = kemoview_allocate_single_viwewer_struct();
	kemoview_set_stereo_shutter(iflag_streo_shutter);
	
	if(iflag_streo_shutter == SHUTTER_ON){
		kemoview_set_anaglyph_flag(ANAGLYPH_OFF);
	} else {
		kemoview_set_anaglyph_flag(ANAGLYPH_ON);
	};
	
	link_glut_menu_address();
	glutInit(&narg_glut, arg_glut);
	
	/*! GTK Initialization*/
	/* gtk_set_locale(); */
	gtk_init (&narg_glut, &arg_glut);
    
	/*! Initializations with GLUT*/
	if(iflag_streo_shutter == SHUTTER_ON){
		glutInitDisplayMode(GLUT_RGBA|GLUT_DOUBLE|GLUT_DEPTH
					|GLUT_MULTISAMPLE|GLUT_STEREO|GLUT_3_2_CORE_PROFILE);
		} else {
		glutInitDisplayMode(GLUT_RGBA|GLUT_DOUBLE|GLUT_DEPTH
					|GLUT_MULTISAMPLE|GLUT_3_2_CORE_PROFILE);
	};
	/*! Create viewer window*/
    kemoview_set_retinamode(IZERO);
	kemoview_set_windowsize(NPIX_X, NPIX_Y);
	glutInitWindowSize(NPIX_X, NPIX_Y);
	winid = create_kemoview_window();
	
	  fprintf(
	  stdout,
	  "INFO: OpenGL Version: %s\n",
	  glGetString(GL_VERSION)
	  );
	
	
	/*  initialize view_modifier, receiving the id for it's submenu  */
	kemoviewer_reset_to_init_angle();
	view_modifier_init();
	
	/* ! set the perspective and lighting */
	kemoview_init_background_color();
	kemoview_init_lighting(iflag_core_profile);
	kemoview_init_phong_light_list();
	
	menu_win = create_kemoview_menu();
	
	glutSetWindow(menu_win);
	kemoview_draw_menu_setup();
	
	glutSetWindow(winid);
	draw_mesh_w_menu();
	
	/*! set callback for GLUT*/
	glutMainLoop();
	return;
};

