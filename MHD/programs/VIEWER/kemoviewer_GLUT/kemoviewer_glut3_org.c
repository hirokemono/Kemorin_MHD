
/* kemo_mesh_viewer_glut.h*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "kemoviewer.h"
#include "view_modifier_glut.h"
#include "const_viewer_menu_glut.h"
#include "kemoview_glut_console_input.h"
#include "kemoview_glut_routines.h"


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

void set_window_id_glut3(int *id3_window, int *id3_menu){
	id3_window = &winid;
	id3_menu = &menu_win;
	set_main_window_id_glut(winid);
	return;
}

void link_glut_menu_address(){
	glut_menu_id = &glut_menu_id_struct;
	return;
}


/* draw object using GLUT */

void draw_mesh_w_menu(){
	make_1st_level_menu();
	draw_mesh_keep_menu();
	return;
};

static void save_image_handler(){
	char image_head[LENGTHBUF];
    struct kv_string *image_prefix;
    int id_image;
	
	input_file_header(image_head);
    id_image = input_image_format();
    image_prefix = kemoview_init_kvstring_by_string(image_head);
	
	glutSetWindow(winid);
	draw_mesh_keep_menu();
    kemoview_write_window_to_file(id_image, image_prefix);
    kemoview_free_kvstring(image_prefix);
    return;
};

static void load_texture_handler(){
	char image_head[LENGTHBUF];
    int id_image;
    struct kv_string *image_prefix;
    
	input_file_header(image_head);
    image_prefix = kemoview_init_kvstring_by_string(image_head);
    id_image = input_image_format();
	
	if(id_image == SAVE_PNG || id_image == SAVE_BMP){
	
		kemoview_set_texture_to_PSF(id_image, image_prefix);
		kemoview_set_PSF_patch_color_mode(TEXTURED_SURFACE);
	};
    kemoview_free_kvstring(image_prefix);
	
	glutSetWindow(winid);
	draw_mesh_w_menu();
	return;
};

static void save_evolution_handler(){
	int ist_udt, ied_udt, inc_udt;
	int iflag;
    struct kv_string *image_prefix = kemoview_alloc_kvstring();
	
	ist_udt = kemoview_get_PSF_full_path_file_prefix(image_prefix, &iflag);
	int id_image = input_image_format();
	read_psf_evolution_steps(&ist_udt, &ied_udt, &inc_udt);
	
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

void save_rot_image_handler(int id_rot){
    int inc_deg = 2;
	char image_head[LENGTHBUF];
    struct kv_string *image_prefix;
	
	inc_deg = read_psf_rotation_increment();
	input_file_header(image_head);
    image_prefix = kemoview_init_kvstring_by_string(image_head);
	int id_image = input_image_format();
	write_rotate_views_glut(id_image, image_prefix, id_rot, inc_deg);
    kemoview_free_kvstring(image_prefix);
};


/* ---------  Action for selected menu -----------   */ 

static void read_draw_kemoview_data_glut(){
	read_kemoview_data_glut();
	draw_mesh_w_menu();
	return;
};

static void kemoview_psf_draw_input_setting(int selected){
	if      (selected == ISET_RANGE)       {
		set_psf_range_console();
	}
	else if (selected == ISET_NLINE)       {
		set_num_isoline();
	}
	else if (selected == ISET_PSF_OPACITY) {
		set_psf_opacity();
	}
	else if (selected == ISET_PSF_VEC_INC) {
		set_psf_vector_increment();
	}
	else if (selected == ISET_PSF_REFVECT) {
		set_psf_vector_scale();
	}
	else if (selected == ISET_PSF_V_THICK) {
		set_psf_vector_thickness();
	};
	return;
};

static void kemoview_fline_draw_setting(int sel){
	
	printf("Fieldline menu selected %d \n",sel);
	if (sel == ISET_RANGE) {set_fline_range_console();}
	else if( sel == ISET_FLINE_THICK) {set_fline_thick_console();};

	return;
};


static void main_menu_handler(int sel){
	if (sel == QUIT_SELECTED)   { exit(EXIT_SUCCESS); }
	else if(sel == FILE_OPEN)  { read_draw_kemoview_data_glut(); }
	else if(sel == SAVE_SNAPSHOT)  { save_image_handler(); }
	else if(sel == SAVE_EVOLUTION) { save_evolution_handler(); }
    else if(sel == SET_BACKGROUND) { set_background_color_console(); }
	return;
};

static void viewtype_handler(int sel){
	set_viewtype_mode_glut(sel, viewtype_title);
	draw_mesh_w_menu();
	return;
}

static void domain_handler(int sel){
	if(sel == MESH_OFF){
		kemoview_close_mesh_view();
	} else{
		kemoview_mesh_draw_toggle(sel);
	};
	draw_mesh_w_menu();
	return;
}


static void domain_color_handler(int sel){
	if (sel == SET_OPACITY) {
		set_domain_opacity();
	} else {
		kemoview_set_domain_color_flag(SURFSOLID_TOGGLE, sel);
	};
	draw_mesh_keep_menu();
	return;
;
};
static void domain_grid_color_handler(int sel){
	kemoview_set_domain_color_flag(SURFGRID_TOGGLE, sel);
	draw_mesh_keep_menu();
};
static void domain_node_color_handler(int sel){
	kemoview_set_domain_color_flag(SURFNOD_TOGGLE, sel);
	draw_mesh_keep_menu();
};


static void node_node_color_handler(int sel){
	kemoview_set_node_grp_color_flag(sel);
	draw_mesh_keep_menu();
};


static void ele_surf_color_handler(int sel){
	if(sel == SET_OPACITY){
		set_ele_group_opacity();
	} else {
		kemoview_set_ele_grp_color_flag(SURFSOLID_TOGGLE, sel);
	};
	draw_mesh_keep_menu();
	return;
};
static void ele_grid_color_handler(int sel){
	kemoview_set_ele_grp_color_flag(SURFGRID_TOGGLE, sel);
	draw_mesh_keep_menu();
};
static void ele_node_color_handler(int sel){
	kemoview_set_ele_grp_color_flag(SURFNOD_TOGGLE, sel);
	draw_mesh_keep_menu();
};


static void surf_surf_color_handler(int sel){
	if (sel == SET_OPACITY) {
		set_surf_group_opacity();
	} else {
		kemoview_set_surf_grp_color_flag(SURFSOLID_TOGGLE, sel);
	};
	draw_mesh_keep_menu();
};
static void surf_grid_color_handler(int sel){
	kemoview_set_surf_grp_color_flag(SURFGRID_TOGGLE, sel);
	draw_mesh_keep_menu();
};
static void surf_node_color_handler(int sel){
	kemoview_set_surf_grp_color_flag(SURFNOD_TOGGLE, sel);
	draw_mesh_keep_menu();
};


static void surf_4_ele_grp_handler(int sel){
	kemoview_ele_grp_toggle(sel);
	draw_mesh_w_menu();
};

static void grid_4_ele_grp_handler(int sel){
	kemoview_ele_grp_grid_toggle(sel);
	draw_mesh_w_menu();
};

static void nod_4_ele_grp_handler(int sel){
	kemoview_ele_grp_nod_toggle(sel);
	draw_mesh_w_menu();
};


static void surf_4_surf_grp_handler(int sel){
	kemoview_surf_grp_toggle(sel);
	draw_mesh_w_menu();
};

static void grid_4_surf_grp_handler(int sel){
	kemoview_surf_grp_grid_toggle(sel);
	draw_mesh_w_menu();
};

static void nod_4_surf_grp_handler(int sel){
	kemoview_surf_grp_nod_toggle(sel);
	draw_mesh_w_menu();
};


static void nod_grp_handler(int sel){
	kemoview_nod_grp_toggle(sel);
	draw_mesh_w_menu();
};

static void psf_handler(int sel){
    int nload_psf, toggle;
    
	if (sel == PSF_OFF) {
        set_viewtype_mode_glut(VIEW_3D, viewtype_title);
        nload_psf = kemoview_close_PSF_view();
		draw_mesh_w_menu();
	}
	else {
		toggle = kemoview_select_PSF_draw_switch(sel);
		kemoview_psf_draw_input_setting(sel);
		draw_mesh_w_menu();
	};
	return;
};

static void psf_colormap_handler(int sel){
	if(sel == WRITE_CMAP){
		save_PSF_colormap_file_glut();
	} else if(sel == READ_CMAP){
		load_PSF_colormap_file_glut();
		draw_mesh_w_menu();
	};
	return;
};

static void fline_handler(int sel){
	int toggle;
	if (sel == FLINE_OFF) {kemoview_close_fieldline_view();}
	else if (sel == ISET_FLINE_TYPE) {toggle = kemoview_toggle_fline_type();}
	else {kemoview_fline_draw_setting(sel);};
	
	draw_mesh_w_menu();
	return;
};

static void set_current_psf_handler(int sel){
	kemoview_set_current_PSF(sel);
	draw_mesh_w_menu();
	return;
};

static void set_psf_field_handler(int sel){
	kemoview_set_PSF_field(sel);
	draw_mesh_w_menu();
	return;
};

static void set_psf_comp_handler(int sel){
	kemoview_set_PSF_component(sel);
	draw_mesh_w_menu();
	return;
};

static void set_psf_patchcolor_handler(int sel){
	if (sel == WHITE_PSF_SURF)          {kemoview_set_PSF_patch_color_mode(WHITE_SURFACE);}
    else if (sel == SGL_COLOR_PSF_SURF) {set_psf_single_color_console();}
	else if (sel == RAINBOW_PSF_SURF)   {kemoview_set_PSF_patch_color_mode(RAINBOW_SURFACE);}
	else if (sel == TEXTURE_PSF_SURF) {load_texture_handler();};
	
	draw_mesh_w_menu();
	return;
};

static void set_psf_linecolor_handler(int sel){
	if (sel == BLACK_PSF_LINE)          {kemoview_set_PSF_isoline_color_mode(BLACK_LINE);}
	else if (sel == RAINBOW_PSF_LINE)   {kemoview_set_PSF_isoline_color_mode(RAINBOW_LINE);}
	else if (sel == WHITE_PSF_LINE)     {kemoview_set_PSF_isoline_color_mode(WHITE_LINE);};
	
	draw_mesh_w_menu();
	return;
};

static void set_psf_colormode_handler(int sel){
    kemoview_set_PSF_color_mode(sel);
    draw_mesh_w_menu();
    return;
};

static void add_psf_colormap_handler(int sel){
	if (sel == ADD_PSF_COLOR) {
		add_psf_colormap_point_console();
		draw_mesh_w_menu();
	}
    return;
};

static void modify_psf_colormap_handler(int sel){
	modify_psf_colormap_point_console(sel);
	draw_mesh_w_menu();
    return;
};

static void delete_psf_colormap_handler(int sel){
	kemoview_delete_PSF_color_list(sel);
	kemoview_check_PSF_colormap_control();
	draw_mesh_w_menu();
   return;
};


static void add_psf_opacitymap_handler(int sel){
	if (sel == ADD_PSF_OPACITY){
		add_psf_opacitymap_point_console();
		draw_mesh_w_menu();
	}
    return;
};

static void modify_psf_opacitymap_handler(int sel){
	modify_psf_opacitymap_point_console(sel);
	draw_mesh_w_menu();
    return;
};

static void delete_psf_opacitymap_handler(int sel){
	kemoview_delete_PSF_opacity_list(sel);
	kemoview_check_PSF_colormap_control();
	draw_mesh_w_menu();
   return;
};


static void set_fline_color_handler(int sel){
	kemoview_set_fline_color_field(sel);
	draw_mesh_w_menu();
	return;
};

static void set_fline_c_comp_handler(int sel){
	kemoview_set_fline_color_component(sel);
	draw_mesh_w_menu();
	return;
};

static void set_fline_col_type_handler(int sel){
	kemoview_set_fline_color_type(sel);
	draw_mesh_w_menu();
	return;
};


static void color_mode_handler(int sel){
	if (sel == GRAYSCALE) {
		kemoview_set_mesh_color_mode(GRAYSCALE);
	} else if( sel == RAINBOW_COLOR) {
		kemoview_set_mesh_color_mode(RAINBOW_COLOR);
	} else if( sel == SET_NUM_COLORS) {
		set_num_color_loop_console();
	};

	draw_mesh_w_menu();
	return;
}

static void object_property_handler(int sel){
	int toggle;
    toggle = kemoview_toggle_object_properties(sel);

	if		 ( sel == SET_NODE_SIZE) {
		set_node_size_console();
	} else if( sel == SET_DISTANCE_DOMAIN) {
		set_domain_distance_console();
		kemoview_draw_with_modified_domain_distance();
	} else if( sel == SET_COAST_RADIUS) {
		set_coastline_radius_console();
	} else if( sel == OUTPUT_V_MATRIX) {
		save_viewmatrix_file();
	} else if( sel == INPUT_V_MATRIX) {
		load_viewmatrix_file();
	};
	draw_mesh_w_menu();
	return;
}


static void dummy_handler(int sel){
	int itmp;
	itmp = sel;
	return;
}


/* 3rd level menues*/

static void make_3rd_level_mesh_menu(){
	
	glut_menu_id->surface_color_menu = glutCreateMenu(domain_color_handler);
	glut_surf_color_menu_item();
	glut_menu_id->grid_color_menu = glutCreateMenu(domain_grid_color_handler);
	glut_line_color_menu_item();
	glut_menu_id->node_color_menu = glutCreateMenu(domain_node_color_handler);
	glut_color_menu_item();
	
	
	glut_menu_id->node_node_color_menu = glutCreateMenu(node_node_color_handler);
	glut_grp_color_menu_item();
	
	
	glut_menu_id->surf_4_ele_grp_menu = glutCreateMenu(surf_4_ele_grp_handler);
	glut_ele_grp_patch_menu();
	
	glut_menu_id->grid_4_ele_grp_menu = glutCreateMenu(grid_4_ele_grp_handler);
	glut_ele_grp_edge_menu();
	
	glut_menu_id->nod_4_ele_grp_menu =  glutCreateMenu(nod_4_ele_grp_handler);
	glut_ele_grp_node_menu();
	
	glut_menu_id->ele_surf_color_menu = glutCreateMenu(ele_surf_color_handler);
	glut_surf_color_menu_item();
	
	glut_menu_id->ele_grid_color_menu = glutCreateMenu(ele_grid_color_handler);
	glut_line_color_menu_item();
	glutAddMenuEntry("Color by group",GROUP_COLOR);
	
	glut_menu_id->ele_node_color_menu = glutCreateMenu(ele_node_color_handler);
	glut_grp_color_menu_item();
	
	
	glut_menu_id->surf_4_surf_grp_menu = glutCreateMenu(surf_4_surf_grp_handler);
	glut_surf_grp_patch_menu();
	
	glut_menu_id->grid_4_surf_grp_menu = glutCreateMenu(grid_4_surf_grp_handler);
	glut_surf_grp_edge_menu();
	
	glut_menu_id->nod_4_surf_grp_menu =  glutCreateMenu(nod_4_surf_grp_handler);
	glut_surf_grp_node_menu();
	
	glut_menu_id->surf_surf_color_menu = glutCreateMenu(surf_surf_color_handler);
	glut_surf_color_menu_item();
	
	glut_menu_id->surf_grid_color_menu = glutCreateMenu(surf_grid_color_handler);
	glut_line_color_menu_item();
	glutAddMenuEntry("Color by group",GROUP_COLOR);
	
	glut_menu_id->surf_node_color_menu = glutCreateMenu(surf_node_color_handler);
	glut_grp_color_menu_item();
	
};

/* 2nd level menues*/

static void make_2nd_level_view_menu(){
	glut_menu_id->viewtype_id = glutCreateMenu(viewtype_handler);
	glut_viewtype_menu();
	
	return;
};

static void make_2nd_level_mesh_menu(){
	
	glut_menu_id->domain_id =   glutCreateMenu(domain_handler);
	glut_mesh_display_menu();
	glutAddSubMenu("Surface color",   glut_menu_id->surface_color_menu);
	glutAddSubMenu("Wireframe color", glut_menu_id->grid_color_menu);
	glutAddSubMenu("Node color",      glut_menu_id->node_color_menu);
	
	glutAddMenuEntry("Close mesh",    MESH_OFF);
	
	
	glut_menu_id->nod_grp_menu = glutCreateMenu(nod_grp_handler);
	glut_nod_grp_menu_item();
	glutAddSubMenu("Node color",         glut_menu_id->node_node_color_menu);
	
	glut_menu_id->ele_grp_menu = glutCreateMenu(dummy_handler);
	glutAddSubMenu("Show/hide surface",  glut_menu_id->surf_4_ele_grp_menu);
	glutAddSubMenu("Show/hide wireframe",glut_menu_id->grid_4_ele_grp_menu);
	glutAddSubMenu("Show/hide node",     glut_menu_id->nod_4_ele_grp_menu);
	glutAddSubMenu("Surface color",      glut_menu_id->ele_surf_color_menu);
	glutAddSubMenu("Wireframe color",    glut_menu_id->ele_grid_color_menu);
	glutAddSubMenu("Node color",         glut_menu_id->ele_node_color_menu);
	
	glut_menu_id->surf_grp_menu = glutCreateMenu(dummy_handler);
	glutAddSubMenu("Show/hide surface",  glut_menu_id->surf_4_surf_grp_menu);
	glutAddSubMenu("Show/hide wireframe",glut_menu_id->grid_4_surf_grp_menu);
	glutAddSubMenu("Show/hide node",     glut_menu_id->nod_4_surf_grp_menu);
	glutAddSubMenu("Surface color",      glut_menu_id->surf_surf_color_menu);
	glutAddSubMenu("Wireframe color",    glut_menu_id->surf_grid_color_menu);
	glutAddSubMenu("Node color",         glut_menu_id->surf_node_color_menu);
	
	return;
};

/* 4th level menues*/
static void make_4th_level_psf_menu(){
    int iflag_solid = kemoview_get_PSF_draw_flags(PSFSOLID_TOGGLE);
    int iflag_grid =  kemoview_get_PSF_draw_flags(PSFGRID_TOGGLE);
    int i, npoint;
    double value, color, opacity;
    char tmp_menu[1024];
    
    if(iflag_solid > 0 || iflag_grid > 0){
        npoint = kemoview_get_PSF_color_table_num();
        glut_menu_id->modify_colormap_menu =  glutCreateMenu(modify_psf_colormap_handler);
        for(i = 0; i < npoint; i++) {
            kemoview_get_PSF_color_items(i, &value, &color);
            sprintf(tmp_menu, "data:%3.2e, color:%.2f", value, color);
            glutAddMenuEntry(tmp_menu,  i);
        };
        
        glut_menu_id->delete_colormap_menu =  glutCreateMenu(delete_psf_colormap_handler);
        for(i = 0; i < npoint; i++) {
            kemoview_get_PSF_color_items(i, &value, &color);
            sprintf(tmp_menu, "data:%3.2e, color:%.2f", value, color);
            glutAddMenuEntry(tmp_menu,  i);
        };
        
        
        npoint = kemoview_get_PSF_opacity_table_num();
        glut_menu_id->modify_opacitymap_menu =  glutCreateMenu(modify_psf_opacitymap_handler);
        for(i = 0; i < npoint; i++) {
            kemoview_get_PSF_opacity_items(i, &value, &opacity);
            sprintf(tmp_menu, "data:%3.2e, color:%.2f", value, opacity);
            glutAddMenuEntry(tmp_menu,  i);
        };
        
        glut_menu_id->delete_opacitymap_menu =  glutCreateMenu(delete_psf_opacitymap_handler);
        for(i = 0; i < npoint; i++) {
            kemoview_get_PSF_opacity_items(i, &value, &opacity);
            sprintf(tmp_menu, "data:%3.2e, color:%.2f", value, opacity);
            glutAddMenuEntry(tmp_menu,  i);
		};
		
		glut_menu_id->ichoose_psf_colormode_menu = glutCreateMenu(set_psf_colormode_handler);
		glut_PSF_colormode_select();
		
        glut_menu_id->ichoose_psf_colormap_menu =  glutCreateMenu(add_psf_colormap_handler);
        glutAddMenuEntry("Add feature point",  ADD_PSF_COLOR);
        glutAddSubMenu("Modify feature point",  glut_menu_id->modify_colormap_menu);
        glutAddSubMenu("Delete feature point",  glut_menu_id->delete_colormap_menu);
        
        glut_menu_id->ichoose_psf_opacitymap_menu =  glutCreateMenu(add_psf_opacitymap_handler);
        glutAddMenuEntry("Add feature point",  ADD_PSF_OPACITY);
        glutAddSubMenu("Modify feature point",  glut_menu_id->modify_opacitymap_menu);
        glutAddSubMenu("Delete feature point",  glut_menu_id->delete_opacitymap_menu);
	};
	return;
};

/* 3rd level menues*/
static void make_3rd_level_psf_menu(){
	
	int num_psf =     kemoview_get_PSF_num_loaded();
	int num_fld =     kemoview_get_PSF_num_field();
	int if_psf =      kemoview_get_PSF_field_id();
	int num_comp =    kemoview_get_PSF_num_component(if_psf);
	int iflag_solid = kemoview_get_PSF_draw_flags(PSFSOLID_TOGGLE);
	int iflag_grid =  kemoview_get_PSF_draw_flags(PSFGRID_TOGGLE);
	
	if(num_psf > 1){
		glut_menu_id->ichoose_current_psf_menu = glutCreateMenu(set_current_psf_handler);
		glut_current_PSF_select();
	};
	
	if (num_fld > 1) {
		glut_menu_id->ichoose_field_menu = glutCreateMenu(set_psf_field_handler);
		glut_PSF_field_select();
	};
	
	if (num_comp > 1) {
		glut_menu_id->ichoose_comp_menu = glutCreateMenu(set_psf_comp_handler);
		glut_PSF_comps_select();
	};

	if (iflag_solid > 0) {
		glut_menu_id->ichoose_psf_patchcolor_menu = glutCreateMenu(set_psf_patchcolor_handler);
		glut_PSF_patchcolor_select();
	};

	if (iflag_grid > 0) {
		glut_menu_id->ichoose_psf_linecolor_menu =  glutCreateMenu(set_psf_linecolor_handler);
		glut_PSF_linecolor_select();
	};

    if(iflag_solid > 0 || iflag_grid > 0){
        glut_menu_id->ichoose_psf_color_menu = glutCreateMenu(psf_colormap_handler);
        glutAddSubMenu("Colormap mode", glut_menu_id->ichoose_psf_colormode_menu);
        glutAddSubMenu("Color map",    glut_menu_id->ichoose_psf_colormap_menu);
        glutAddSubMenu("Opacitiy map", glut_menu_id->ichoose_psf_opacitymap_menu);
		glutAddMenuEntry("Save colormap file", WRITE_CMAP);
        glutAddMenuEntry("Read colormap file", READ_CMAP);
    };

	return;
};


static void make_3rd_level_fline_menu(){
	
	int num_fld =  kemoview_get_fline_color_num_field();
	int if_fline = kemoview_get_fline_color_field();
	int num_comp = kemoview_get_fline_color_num_comps(if_fline);
	
	if (num_fld > 1) {
		glut_menu_id->ichoose_fline_c_menu = glutCreateMenu(set_fline_color_handler);
		glut_fline_color_select();
	};
	
	if (num_comp > 1) {
		glut_menu_id->ichoose_comp_menu = glutCreateMenu(set_fline_c_comp_handler);
		glut_fline_color_comp_select();
	};
	
	glut_menu_id->ichoose_fline_col_type_menu = glutCreateMenu(set_fline_col_type_handler);
	glut_fline_col_type_menu();
	return;
};

/* 2nd level menues*/

static void make_2nd_level_psf_menu(){
	char tmp_menu[1024];
    struct kv_string *stripped_filehead;
    struct kv_string *colorname;
    int istep;
	
	int num_psf =     kemoview_get_PSF_num_loaded();
	int num_fld =     kemoview_get_PSF_num_field();
	int if_psf =      kemoview_get_PSF_field_id();
	int ic_psf =      kemoview_get_PSF_component_id();
	int num_comp =    kemoview_get_PSF_num_component(if_psf);
	int iflag_solid = kemoview_get_PSF_draw_flags(PSFSOLID_TOGGLE);
	int iflag_grid =  kemoview_get_PSF_draw_flags(PSFGRID_TOGGLE);
	
	glut_menu_id->psf_root_menu = glutCreateMenu(psf_handler);
	
	if(num_psf > 1){
        stripped_filehead = kemoview_alloc_kvstring();
		istep = kemoview_get_PSF_file_prefix(stripped_filehead);
		sprintf(tmp_menu, "Current: %s", stripped_filehead->string);
        kemoview_free_kvstring(stripped_filehead);
		glutAddSubMenu(tmp_menu, glut_menu_id->ichoose_current_psf_menu);
	}
    
    colorname = kemoview_alloc_kvstring();
	kemoview_get_PSF_field_name(colorname,if_psf);
	if (num_fld > 1) {
		glutAddSubMenu(colorname->string, glut_menu_id->ichoose_field_menu);
	} else {
		glutAddMenuEntry(colorname->string, PSF_NOTHING_TODO);
	};
    kemoview_free_kvstring(colorname);
	
	if (num_comp > 1) {
		set_PSF_component_name(num_comp,ic_psf,tmp_menu); 
		glutAddSubMenu(tmp_menu, glut_menu_id->ichoose_comp_menu);
	};
	
	glut_PSF_draw_menu();

	if(iflag_solid > 0){glutAddSubMenu("Surface  color", glut_menu_id->ichoose_psf_patchcolor_menu);};
	if(iflag_grid > 0) {glutAddSubMenu("Line color", glut_menu_id->ichoose_psf_linecolor_menu);};
	
	glut_PSF_range_menu();
    
    if(iflag_solid > 0 || iflag_grid > 0){
        glutAddSubMenu("Color and Opacity", glut_menu_id->ichoose_psf_color_menu);
    }
	
	glutAddMenuEntry("Close Current PSF data", PSF_OFF);
    return;
};

static void make_2nd_level_fline_menu(){
	char tmp_menu[1024];
	
	int num_fld =  kemoview_get_fline_color_num_field();
	int if_fline = kemoview_get_fline_color_field();
	int ic_fline = kemoview_get_fline_color_component();
	int num_comp = kemoview_get_fline_color_num_comps(if_fline);
	int itype_fline = kemoview_get_fline_type();
    struct kv_string *colorname = kemoview_alloc_kvstring();
	
	glut_menu_id->fline_root_menu = glutCreateMenu(fline_handler);
	
	kemoview_get_fline_color_data_name(tmp_menu, if_fline);
	if(num_fld > 1){
		glutAddSubMenu(colorname->string, glut_menu_id->ichoose_fline_c_menu);
	} else {
		glutAddMenuEntry(colorname->string, PSF_NOTHING_TODO);
	};
    kemoview_free_kvstring(colorname);
	
	if (num_comp > 1) {
		sprintf(tmp_menu, "Current component: %d", (ic_fline+1) ); 
		glutAddSubMenu(tmp_menu, glut_menu_id->ichoose_comp_menu);
	};
	
	glutAddSubMenu("Line color type", glut_menu_id->ichoose_fline_col_type_menu);

	if (itype_fline == IFLAG_PIPE) {
		glutAddMenuEntry("Draw lines",          ISET_FLINE_TYPE);
	} else {
		glutAddMenuEntry("Draw tubes",          ISET_FLINE_TYPE);
	}

	glutAddMenuEntry("Set line thickness",     ISET_FLINE_THICK);
	glutAddMenuEntry("Set range",              ISET_RANGE);
	
	glutAddMenuEntry("Delete Fieldline data",   FLINE_OFF);
	
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
	
	glut_draw_axis_menu_item(iflag_axis);
	glut_draw_coast_menu_item(iflag_draw_coast);
	glut_draw_sph_grid_menu_item(iflag_draw_sph);
	if ( (iflag_draw_coast != 0) || (iflag_draw_sph != 0)) {
		glutAddMenuEntry("Set Coastline radius",SET_COAST_RADIUS);
	};
	
	glut_drawing_select();
	if ( (iflag_draw_m > 0) && (iflag_draw_p == 0) && (iflag_draw_f == 0) ) {
		glutAddMenuEntry("Object distance",SET_DISTANCE_DOMAIN);
	};

	glutAddMenuEntry("Output transfer matrices",OUTPUT_V_MATRIX);
	glutAddMenuEntry("Load transfer matrices",INPUT_V_MATRIX);
	
	glut_menu_id->save_rot_image_menu = glutCreateMenu(save_rot_image_handler);
	glutAddMenuEntry("x-axis",ROTATE_X);
	glutAddMenuEntry("y-axis",ROTATE_Y);
	glutAddMenuEntry("z-axis",ROTATE_Z);
	
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
	
	if( iflag_draw_m > 0){
		make_3rd_level_mesh_menu();
		make_2nd_level_mesh_menu();
	};
	
	if( iflag_draw_p > 0){
        make_4th_level_psf_menu();
		make_3rd_level_psf_menu();
		make_2nd_level_psf_menu();
	};
	
	if( iflag_draw_f > 0){
		make_3rd_level_fline_menu();
		make_2nd_level_fline_menu();
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
	
	if( iflag_draw_m > 0){
		glutAddSubMenu("Domain informations",glut_menu_id->domain_id);
		glutAddSubMenu("Node groups",glut_menu_id->nod_grp_menu);
		glutAddSubMenu("Element_groups",glut_menu_id->ele_grp_menu);
		glutAddSubMenu("Surface_groups",glut_menu_id->surf_grp_menu);
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
		glutAddSubMenu("Save rotate images", glut_menu_id->save_rot_image_menu);
		glutAddSubMenu("Rotate on Window",   glut_menu_id->draw_rot_image_menu);
	};

    glutAddMenuEntry("Background color",SET_BACKGROUND);

	glutAddMenuEntry("Quit",QUIT_SELECTED);
	glutAttachMenu(GLUT_LEFT_BUTTON);
	return;
};

/* Main routine for C */

void draw_mesh_kemo(int iflag_streo_shutter, int iflag_dmesh) {
	int narg_glut = 0;
	char **arg_glut;
	int iflag_core_profile = 1;
	
	/* Initialize arrays for viewer */
	
	kemoview_allocate_single_viwewer_struct(single_kemoview);
	kemoview_set_stereo_shutter(iflag_streo_shutter);
	
	if(iflag_streo_shutter == SHUTTER_ON){
		kemoview_set_anaglyph_flag(ANAGLYPH_OFF);
	} else {
		kemoview_set_anaglyph_flag(ANAGLYPH_ON);
	};
	
	link_glut_menu_address();
	
	/*! Initializations with GLUT*/
	glutInit(&narg_glut, arg_glut);
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
	winid = glutCreateWindow("Kemoviewer");
	set_main_window_id_glut(winid);
	
	/*glutEntryFunc(enter_leave);*/
	  fprintf(
	  stdout,
	  "INFO: OpenGL Version: %s\n",
	  glGetString(GL_VERSION)
	  );
	
	/*! Set the display callback  */
	
	glutDisplayFunc(display);
	glutReshapeFunc(modifywindow);
	
	/*  initialize view_modifier, receiving the id for it's submenu  */
	kemoviewer_reset_to_init_angle();
	view_modifier_init();
	
	/* ! set the perspective and lighting */
	kemoview_init_background_color();
	kemoview_init_lighting(iflag_core_profile);
	
	/*! Create menu window*/
	menu_win = glutCreateSubWindow(winid,IZERO,IZERO,MENU_WIDTH,MENU_HEIGHT);
	glutDisplayFunc(display_menu);
	
	draw_mesh_w_menu();
	
	/*! set callback for GLUT*/
	glutMainLoop();
	return;
};


int main(int argc, char *argv[]){
	int iflag_streo_shutter = SHUTTER_OFF;
	int i;
	
	/*	printf("Number of arguments %d\n", argc);*/
	for (i = 0; i < argc; i++) {
/*		printf("%dth arguments: %s\n", i, argv[i]);*/
		if(strcmp(argv[i],"-help") == 0){
			printf("-stereo_shutter: Use streo monitor with shutter\n");
			return 0;
		}
	}
		
	for (i = 0; i < argc; i++) {
		if(strcmp(argv[i],"-stereo_shutter") == 0){
			printf("shutter ON\n");
			iflag_streo_shutter = SHUTTER_ON;
		}
	}
	
	draw_mesh_kemo(iflag_streo_shutter, IZERO);
	return 0;
};
