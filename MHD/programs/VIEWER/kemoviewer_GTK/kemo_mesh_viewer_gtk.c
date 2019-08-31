
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
		gtk_fieldline_menu();
	}
	else if(sel == SET_COAST_RADIUS){gtk_main_menu(single_kemoview);}
    else if(sel == SET_BACKGROUND) {
		gtk_BGcolorselect(single_kemoview);
		draw_mesh_keep_menu();
	};
    return;
};

static void dummy_handler(int sel){
	int itmp;
	itmp = sel;
	return;
}

/* Create 1st level menu() */

static void make_1st_level_menu(){
	GLint menu_id;
	
	int iflag_draw_m = kemoview_get_draw_mesh_flag();
	int iflag_draw_p = kemoview_get_PSF_draw_switch();
	int iflag_draw_f = kemoview_get_fline_switch();
	int iflag_any_objects_on = iflag_draw_p + iflag_draw_m + iflag_draw_f;
	
	int nload_psf = kemoview_get_PSF_num_loaded();
	
	glutSetWindow(menu_win);
	
	glut_menu_id->submenu_id = menu_init();
	
	menu_id = glutCreateMenu(main_menu_handler);
	
	glutAddMenuEntry("Open...",FILE_OPEN);
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
	
	
	glutAddMenuEntry("Set objects",SET_COAST_RADIUS);
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

