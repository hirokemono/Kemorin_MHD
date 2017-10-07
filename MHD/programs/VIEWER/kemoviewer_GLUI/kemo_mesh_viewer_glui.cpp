
/* kemo_mesh_viewer_glui.cpp*/

#include "kemo_mesh_viewer_glui.h"

#define NPIX_X  800
#define NPIX_Y  640

struct glut_menu_address  glut_menu_id_struct;
struct glut_menu_address *glut_menu_id;

static int winid, menu_win;
static char viewtype_title[80] = "3D-View";
static char colormap_file_name[LENGTHBUF];

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

/* Routines for file input menu selection */
/* callback subroutine for GLUI */

GLUI *glui;

GLUI_EditText   *editText_pick_sf;
GLUI_String gi_pick_surf;
char pick_surf_command[LENGTHBUF] = "pick_surf_command";

GLUI_TextBox    *currentDir;
GLUI_String text_current;

GLUI_FileBrowser *file_brouser;
GLUI_EditText   *editText_filename;
GLUI_String text_fname;

GLUI_Button *bottunToGo;

static void SetPickSurfaceCB(int val)
{
	gi_pick_surf = editText_pick_sf->get_text();
	sprintf(pick_surf_command, "%s",editText_pick_sf->get_text());
	printf("command name: %s\n", pick_surf_command);
	set_to_pick_surface_command(pick_surf_command);
	glui->sync_live();
}

static void init_kemoview_data_glui(int val){
	char current[LENGTHBUF];
	char file_name[LENGTHBUF];
	char file_head[LENGTHBUF];
	char file_ext[LENGTHBUF];
	int iflag_datatype;
	
	getcwd(current, sizeof(current));
	
	strcpy(file_name, current);
	strcat(file_name, "/");
	strcat(file_name, text_fname.c_str());
	
	get_ext_from_file_name(file_name, file_head, file_ext);
	printf("file name: %s\n", file_name);
	printf("file_head %s\n", file_head);
	printf("file_ext %s\n", file_ext);
	
	iflag_datatype = kemoview_open_data_glut(file_name);
	
	GLUI_Master.close_all();
	draw_mesh_w_menu();
	return;
};

static void load_psf_texture_glui(int sel){
	char current[LENGTHBUF];
	char file_name[LENGTHBUF];
	char file_head[LENGTHBUF];
	char img_ext[LENGTHBUF];
	int ext_fmt;
	
	getcwd(current, sizeof(current));
	
	strcpy(file_name, current);
	strcat(file_name, "/");
	strcat(file_name, text_fname.c_str());
	
	get_ext_from_file_name(file_name, file_head, img_ext);
	ext_fmt = set_image_file_format_id(img_ext);
	
	if(ext_fmt == SAVE_PNG || ext_fmt == SAVE_BMP){
		set_texture_file_to_current_psf(ext_fmt, file_head);
		set_current_psf_patch_color_mode(TEXTURED_SURFACE);
	};
	
	draw_mesh_w_menu();
	
	GLUI_Master.close_all();
	return;
};

static void load_psf_colormap_glui(int sel){
	char current[LENGTHBUF];
	char file_name[LENGTHBUF];
	char file_head[LENGTHBUF];
	char img_ext[LENGTHBUF];
	int ext_fmt;
	
	getcwd(current, sizeof(current));
	
	strcpy(file_name, current);
	strcat(file_name, "/");
	strcat(file_name, text_fname.c_str());
	
	read_current_PSF_colormap_control_file(file_name);
	
	draw_mesh_w_menu();
	
	GLUI_Master.close_all();
	return;
};

static void load_viewmatrix_glui(int sel){
	char current[LENGTHBUF];
	char file_name[LENGTHBUF];
	char file_head[LENGTHBUF];
	char img_ext[LENGTHBUF];
	int ext_fmt;
	
	getcwd(current, sizeof(current));
	
	strcpy(file_name, current);
	strcat(file_name, "/");
	strcat(file_name, text_fname.c_str());
	
	load_modelview_file_glut(file_name);
	
	draw_mesh_w_menu();
	
	GLUI_Master.close_all();
	return;
};


static void save_psf_colormap_glui(int sel){
	char current[LENGTHBUF];
	char file_name[LENGTHBUF];
	char file_head[LENGTHBUF];
	char img_ext[LENGTHBUF];
	int ext_fmt;
	
	getcwd(current, sizeof(current));
	
	strcpy(file_name, current);
	strcat(file_name, "/");
	strcat(file_name, text_fname.c_str());
	
	write_current_PSF_colormap_control_file(file_name);
	GLUI_Master.close_all();
	return;
};


static void save_viewmatrix_glui(int sel){
	char current[LENGTHBUF];
	char file_name[LENGTHBUF];
	char file_head[LENGTHBUF];
	char img_ext[LENGTHBUF];
	int ext_fmt;
	
	getcwd(current, sizeof(current));
	
	strcpy(file_name, current);
	strcat(file_name, "/");
	strcat(file_name, text_fname.c_str());
	
	write_modelview_file_glut(file_name);
	
	draw_mesh_w_menu();
	
	GLUI_Master.close_all();
	return;
};

static void SetFilenameCB(int val)
{
	text_fname = editText_filename->get_text();
	glui->sync_live();
}

static void openFileBrowerCB(int val)
{
	char current[LENGTHBUF];
	getcwd(current, sizeof(current));
	
	text_fname = file_brouser->get_file();
	text_current = current;
	glui->sync_live();
	/*
	int count = send_glui_idle_count();
	if(count <= 5000) init_kemoview_data_glui(val);
	reset_glui_idle_count();
	*/

}

/* Routines for GLUI Interface */

static void set_open_file_menu_glui(){
	char current[LENGTHBUF];
	if(getcwd(current, sizeof(current)) != NULL){
		printf("current dir is %s\n", current);
	}
	text_current = current;
	
	/*! Set GLUI window*/
	glui = GLUI_Master.create_glui("Select input data file", 0, 0, 0);
	currentDir = new GLUI_TextBox(glui, text_current, false, -1);
	editText_filename = new GLUI_EditText( glui, "file name: ", text_fname,
										  -1, SetFilenameCB);
	file_brouser = new GLUI_FileBrowser(glui, "Select file", GLUI_PANEL_RAISED, 
										0,openFileBrowerCB);
	editText_pick_sf = new GLUI_EditText( glui, "Pickup surface command: ", 
										 pick_surf_command, -1, SetPickSurfaceCB);
	bottunToGo =  new GLUI_Button(glui, "Open", -1, init_kemoview_data_glui);
	
	editText_filename->set_w(240);
	file_brouser->set_w(240);
	currentDir->set_w(240);
	currentDir->set_h(20);
	currentDir->disable();
	editText_pick_sf->set_w(240);
	glui->set_main_gfx_window(winid);

	return;
}

static void set_psf_texture_by_glui(int winid){
	char current[LENGTHBUF];
	if(getcwd(current, sizeof(current)) != NULL){
		printf("current dir is %s\n", current);
	}
	text_current = current;
	glui = GLUI_Master.create_glui("Select file for texture", 0, 100, 100);
	currentDir = new GLUI_TextBox(glui, text_current, false, -1);
	editText_filename = new GLUI_EditText( glui, "file name: ", text_fname,
										  -1, SetFilenameCB);
	file_brouser = new GLUI_FileBrowser(glui, "Select file", GLUI_PANEL_RAISED, 
										0,openFileBrowerCB);
	glui->add_button("load!", 0, load_psf_texture_glui);
	
	editText_filename->set_w(240);
	file_brouser->set_w(240);
	currentDir->set_w(240);
	currentDir->set_h(20);
	currentDir->disable();
	glui->set_main_gfx_window(winid);
	return;
}

static void read_PSF_colormap_file_glui(int winid){
	char current[LENGTHBUF];
	if(getcwd(current, sizeof(current)) != NULL){
		printf("current dir is %s\n", current);
	}
	text_current = current;
	glui = GLUI_Master.create_glui("Select colormap file", 0, 100, 100);
	currentDir = new GLUI_TextBox(glui, text_current, false, -1);
	editText_filename = new GLUI_EditText( glui, "File name: ", text_fname,
										  -1, SetFilenameCB);
	file_brouser = new GLUI_FileBrowser(glui, "Select file", GLUI_PANEL_RAISED, 
										0,openFileBrowerCB);
	glui->add_button("load!", 0, load_psf_colormap_glui);
	
	editText_filename->set_w(240);
	file_brouser->set_w(240);
	currentDir->set_w(240);
	currentDir->set_h(20);
	currentDir->disable();
	glui->set_main_gfx_window(winid);
	return;
}

static void load_viewmatrix_file_glui(int winid){
	char current[LENGTHBUF];
	if(getcwd(current, sizeof(current)) != NULL){
		printf("current dir is %s\n", current);
	}
	text_current = current;
	glui = GLUI_Master.create_glui("Select viewmatrix file", 0, 100, 100);
	currentDir = new GLUI_TextBox(glui, text_current, false, -1);
	editText_filename = new GLUI_EditText( glui, "File name: ", text_fname,
										  -1, SetFilenameCB);
	file_brouser = new GLUI_FileBrowser(glui, "Select file", GLUI_PANEL_RAISED, 
										0,openFileBrowerCB);
	glui->add_button("load!", 0, load_viewmatrix_glui);
	
	editText_filename->set_w(240);
	file_brouser->set_w(240);
	currentDir->set_w(240);
	currentDir->set_h(20);
	currentDir->disable();
	glui->set_main_gfx_window(winid);
	return;
}

static void write_PSF_colormap_file_glui(int winid){
	char current[LENGTHBUF];
	if(getcwd(current, sizeof(current)) != NULL){
		printf("current dir is %s\n", current);
	}
	text_current = current;
	glui = GLUI_Master.create_glui("Save colormap", 0, 100, 100);
	currentDir = new GLUI_TextBox(glui, text_current, false, -1);
	editText_filename = new GLUI_EditText( glui, "File name: ", text_fname,
										  -1, SetFilenameCB);
	file_brouser = new GLUI_FileBrowser(glui, "Select file", GLUI_PANEL_RAISED, 
										0,openFileBrowerCB);
	glui->add_button("load!", 0, save_psf_colormap_glui);
	
	editText_filename->set_w(240);
	file_brouser->set_w(240);
	currentDir->set_w(240);
	currentDir->set_h(20);
	currentDir->disable();
	glui->set_main_gfx_window(winid);
	return;
}

static void save_viewmatrix_file_glui(int winid){
	char current[LENGTHBUF];
	if(getcwd(current, sizeof(current)) != NULL){
		printf("current dir is %s\n", current);
	}
	text_current = current;
	glui = GLUI_Master.create_glui("Save view matrix", 0, 100, 100);
	currentDir = new GLUI_TextBox(glui, text_current, false, -1);
	editText_filename = new GLUI_EditText( glui, "File name: ", text_fname,
										  -1, SetFilenameCB);
	file_brouser = new GLUI_FileBrowser(glui, "Select file", GLUI_PANEL_RAISED, 
										0,openFileBrowerCB);
	glui->add_button("load!", 0, save_viewmatrix_glui);
	
	editText_filename->set_w(240);
	file_brouser->set_w(240);
	currentDir->set_w(240);
	currentDir->set_h(20);
	currentDir->disable();
	glui->set_main_gfx_window(winid);
	return;
}

/* ---------  Action for selected menu -----------   */ 

void draw_rot_image_handler(int id_rot){
	write_rotate_views_glut(NO_SAVE_FILE, (char*)  "Kemoviewer", id_rot);
};

static void kemoview_psf_draw_input_setting(int selected){
	if      (selected == ISET_RANGE)       {
		set_psf_range_by_glui(winid);
	}
	else if (selected == ISET_NLINE)       {
		set_num_isoline_from_glui(winid);
	}
	else if (selected == ISET_PSF_OPACITY) {
		set_psf_opacity_by_glui(winid);
	}
	else if (selected == ISET_PSF_VEC_INC) {
		set_psf_vect_increment_glui(winid);
	}
	else if (selected == ISET_PSF_REFVECT) {
		set_psf_vector_scale_by_glui(winid);
	}
	else if (selected == ISET_PSF_V_THICK) {
		set_psf_vector_thick_by_glui(winid);
	};
	return;
};

static void kemoview_fline_draw_setting(int sel){
	
	printf("Fieldline menu selected %d \n",sel);
	if (sel == ISET_RANGE) {set_fline_range_by_glui(winid);}
	else if( sel == ISET_FLINE_THICK) {set_fline_thick_glui(winid);};
	return;
};

static void main_menu_handler(int sel){
	if (sel == QUIT_SELECTED)   { exit(EXIT_SUCCESS); }
	else if (sel == FILE_OPEN)      { set_open_file_menu_glui(); }
	else if (sel == SAVE_SNAPSHOT)  { set_saveimage_menu_glui(winid); }
	else if (sel == SAVE_EVOLUTION) { set_evolution_menu_glui(winid); }
	else if (sel == SAVE_ROTATION)  { set_rotateimages_menu_glui(winid); }
	return;
};

/* Actions by selection */

static void viewtype_handler(int sel){
	set_viewtype_mode_glut(sel, viewtype_title);
	draw_mesh_w_menu();
	return;
}

static void domain_handler(int sel){
	if(sel == MESH_OFF){
		close_mesh_view();
		draw_mesh_w_menu();
	}
	else{
		kemoview_mesh_draw_toggle(sel);
		draw_mesh_w_menu();
	};
	return;
}


static void domain_color_handler(int sel){
	if (sel == SET_OPACITY) {
		set_domain_opacity_by_glui(winid);
	} else {
		set_domain_color_flag(SURFSOLID_TOGGLE, sel);
	};
	return;
	draw_mesh_keep_menu();
};
static void domain_grid_color_handler(int sel){
	set_domain_color_flag(SURFGRID_TOGGLE, sel);
	draw_mesh_keep_menu();
};
static void domain_node_color_handler(int sel){
	set_domain_color_flag(SURFNOD_TOGGLE, sel);
	draw_mesh_keep_menu();
};


static void node_node_color_handler(int sel){
	set_node_grp_color_flag(sel);
	draw_mesh_keep_menu();
};


static void ele_surf_color_handler(int sel){
	if(sel == SET_OPACITY){
		set_ele_group_opacity_by_glui(winid);
	} else {
		set_ele_grp_color_flag(SURFSOLID_TOGGLE, sel);
	};
	return;
	draw_mesh_keep_menu();
};
static void ele_grid_color_handler(int sel){
	set_ele_grp_color_flag(SURFGRID_TOGGLE, sel);
	draw_mesh_keep_menu();
};
static void ele_node_color_handler(int sel){
	set_ele_grp_color_flag(SURFNOD_TOGGLE, sel);
	draw_mesh_keep_menu();
};


static void surf_surf_color_handler(int sel){
	if (sel == SET_OPACITY) {
		set_surf_group_opacity_by_glui(winid);
	} else {
		set_surf_grp_color_flag(SURFSOLID_TOGGLE, sel);
	};
	draw_mesh_keep_menu();
};
static void surf_grid_color_handler(int sel){
	set_surf_grp_color_flag(SURFGRID_TOGGLE, sel);
	draw_mesh_keep_menu();
};
static void surf_node_color_handler(int sel){
	set_surf_grp_color_flag(SURFNOD_TOGGLE, sel);
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
		nload_psf = close_psf_view();
		draw_mesh_w_menu();
	} else {
		toggle = kemoview_psf_draw_switch_select(sel);
		kemoview_psf_draw_input_setting(sel);
		draw_mesh_w_menu();
	};
	return;
};

static void psf_colormap_handler(int sel){
	if(sel == WRITE_CMAP){write_PSF_colormap_file_glui(winid);}
	else if(sel == READ_CMAP){read_PSF_colormap_file_glui(winid);}
	else if (sel == ADD_PSF_COLOR) {edit_psf_colormap_by_glui(winid);}
	else if (sel == ADD_PSF_OPACITY) {
		edit_psf_opacitymap_by_glui(winid);
		draw_mesh_w_menu();
	};
	return;
};


static void fline_handler(int sel){
	int toggle;
	if (sel == FLINE_OFF) {close_fline_view();}
	else if (sel == ISET_FLINE_TYPE) {toggle = toggle_fline_type_flag();}
	else {kemoview_fline_draw_setting(sel);};
	draw_mesh_w_menu();
	return;
};

static void set_current_psf_handler(int sel){
	set_to_current_PSF(sel);
	draw_mesh_w_menu();
	return;
};

static void set_psf_field_handler(int sel){
	set_current_psf_field_flag(sel);
	draw_mesh_w_menu();
	return;
};

static void set_psf_comp_handler(int sel){
	set_current_psf_component_flag(sel);
	draw_mesh_w_menu();
	return;
};

static void set_psf_patchcolor_handler(int sel){
	if (sel == WHITE_PSF_SURF)        {set_current_psf_patch_color_mode(WHITE_SURFACE);}
    else if (sel == SGL_COLOR_PSF_SURF) {set_current_psf_patch_color_mode(SINGLE_COLOR);}
	else if (sel == RAINBOW_PSF_SURF) {set_current_psf_patch_color_mode(RAINBOW_SURFACE);}
	else if (sel == TEXTURE_PSF_SURF) {set_psf_texture_by_glui(winid);};
	
	draw_mesh_w_menu();
	return;
};

static void set_psf_linecolor_handler(int sel){
	if (sel == BLACK_PSF_LINE)          {set_current_isoline_color(BLACK_LINE);}
	else if (sel == WHITE_PSF_LINE)     {set_current_isoline_color(WHITE_LINE);}
	else if (sel == RAINBOW_PSF_LINE)   {set_current_isoline_color(RAINBOW_LINE);};

	draw_mesh_w_menu();
	return;
};

static void set_psf_colormode_handler(int sel){
    set_current_PSF_color_mode_id(sel);
    draw_mesh_w_menu();
    return;
};

static void set_fline_color_handler(int sel){
	set_fline_color_field_flag(sel);
	draw_mesh_w_menu();
	return;
};

static void set_fline_c_comp_handler(int sel){
	set_fline_color_comp_flag(sel);
	draw_mesh_w_menu();
	return;
};

static void set_fline_col_type_handler(int sel){
	set_to_fieldline_color(sel);
	draw_mesh_w_menu();
	return;
};


static void color_mode_handler(int sel){
	if (sel == GRAYSCALE) {
		set_to_mesh_color_mode(GRAYSCALE);
	} else if( sel == RAINBOW_COLOR) {
		set_to_mesh_color_mode(RAINBOW_COLOR);
	} else if( sel == SET_NUM_COLORS) {
		set_num_color_loop_by_glui(winid);
	};
	draw_mesh_w_menu();
	return;
}

static void object_property_handler(int sel){
	int toggle;
    toggle = object_properties_toggle(sel);

	if		 ( sel == SET_NODE_SIZE) {
		set_node_size_by_glui(winid);
	} else if( sel == SET_DISTANCE_DOMAIN) {
		set_domain_distance_by_glui(winid);
		draw_modified_object_distance();
	} else if( sel == SET_COAST_RADIUS) {
		set_coastline_radius_glui(winid);
	} else if( sel == OUTPUT_V_MATRIX) {
		save_viewmatrix_file_glui(winid);
	} else if( sel == INPUT_V_MATRIX) {
		load_viewmatrix_file_glui(winid);
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
	int iflag_solid = send_kemoview_psf_draw_flags(PSFSOLID_TOGGLE);
	int iflag_grid =  send_kemoview_psf_draw_flags(PSFGRID_TOGGLE);
	
	if (iflag_solid > 0 || iflag_grid > 0) {
		glut_menu_id->ichoose_psf_colormode_menu = glutCreateMenu(set_psf_colormode_handler);
		glut_PSF_colormode_select();
	};
	return;
}

/* 3rd level menues*/
static void make_3rd_level_psf_menu(){
	
	int num_psf =     send_num_loaded_PSF();
	int num_fld =     send_nfield_current_psf();
	int if_psf =      send_draw_field_current_psf();
	int num_comp =    send_ncomp_current_psf(if_psf);
	int iflag_solid = send_kemoview_psf_draw_flags(PSFSOLID_TOGGLE);
	int iflag_grid =  send_kemoview_psf_draw_flags(PSFGRID_TOGGLE);
	
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
    
    if (iflag_solid > 0 || iflag_grid > 0) {
        glut_menu_id->ichoose_psf_color_menu = glutCreateMenu(psf_colormap_handler);
        glutAddSubMenu("Colormap mode", glut_menu_id->ichoose_psf_colormode_menu);
		glutAddMenuEntry("Edit Color map",  ADD_PSF_COLOR);
		glutAddMenuEntry("Edit Opacitiy map",  ADD_PSF_OPACITY);
		glutAddMenuEntry("Save colormap file", WRITE_CMAP);
        glutAddMenuEntry("Read colormap file", READ_CMAP);
    };
	return;
};

static void make_3rd_level_fline_menu(){
	
	int num_fld =  send_nfield_fline();
	int if_fline = send_if_draw_fline();
	int num_comp = send_ncomp_fline(if_fline);
	
	if (num_fld > 0) {
		glut_menu_id->ichoose_fline_c_menu = glutCreateMenu(set_fline_color_handler);
		glut_fline_color_select();
	};
	
	if (num_comp > 0) {
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
	char psf_name[LENGTHBUF];
	
	int num_psf =     send_num_loaded_PSF();
	int num_fld =     send_nfield_current_psf();
	int if_psf =      send_draw_field_current_psf();
	int ic_psf =      send_draw_comp_id_current_psf();
	int num_comp =    send_ncomp_current_psf(if_psf);
	int id_coord =    send_coordinate_id_current_psf();
	int iflag_solid = send_kemoview_psf_draw_flags(PSFSOLID_TOGGLE);
	int iflag_grid =  send_kemoview_psf_draw_flags(PSFGRID_TOGGLE);
	
	glut_menu_id->psf_root_menu = glutCreateMenu(psf_handler);
	
	if(num_psf > 1){
		send_current_psf_file_header(psf_name);
		sprintf(tmp_menu, "Current: %s", psf_name);
		glutAddSubMenu(tmp_menu, glut_menu_id->ichoose_current_psf_menu);
	} else {
	};
	
	
	send_current_psf_data_name(tmp_menu,if_psf);
	if (num_fld > 1) {
		glutAddSubMenu(tmp_menu, glut_menu_id->ichoose_field_menu);
	} else {
		glutAddMenuEntry(tmp_menu, PSF_NOTHING_TODO);
	};
	
	if (num_comp > 1) {
		set_PSF_component_name(num_comp,ic_psf,id_coord,tmp_menu); 
		glutAddSubMenu(tmp_menu, glut_menu_id->ichoose_comp_menu);
	};
	
	glut_PSF_draw_menu();
	
	if(iflag_solid > 0){glutAddSubMenu("Surface  color", glut_menu_id->ichoose_psf_patchcolor_menu);};
	if(iflag_grid > 0) {glutAddSubMenu("Line color", glut_menu_id->ichoose_psf_linecolor_menu);};
	
	glut_PSF_range_menu();
	
    if(iflag_solid > 0 || iflag_grid > 0){
        glutAddSubMenu("Color and Opacity", glut_menu_id->ichoose_psf_color_menu);
    };
	
	glutAddMenuEntry("Close Current PSF data", PSF_OFF);
	
	return;
};

static void make_2nd_level_fline_menu(){
	char tmp_menu[1024];
	
	int num_fld =  send_nfield_fline();
	int if_fline = send_if_draw_fline();
	int ic_fline = send_ic_draw_fline();
	int num_comp = send_ncomp_fline(if_fline);
	int itype_fline = send_fline_type_flag();
	
	glut_menu_id->fline_root_menu = glutCreateMenu(fline_handler);
	
	send_fline_data_name(tmp_menu, if_fline);
	if(num_fld > 1){
		glutAddSubMenu(tmp_menu, glut_menu_id->ichoose_fline_c_menu);
	} else {
		glutAddMenuEntry(tmp_menu, PSF_NOTHING_TODO);
	};
	
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
	glutAddMenuEntry("Set range",     ISET_RANGE);
	
	glutAddMenuEntry("Delete Fieldline data",   FLINE_OFF);
	
	return;
};

/* 2nd level menues*/

static void make_2nd_level_image_menu(){
	int iflag_draw_m = send_iflag_draw_mesh();
	int iflag_draw_p = send_iflag_draw_current_psf();
	int iflag_draw_f = send_iflag_draw_fline();
	int iflag_axis =       send_object_property_flags(AXIS_TOGGLE);
	int iflag_draw_coast = send_object_property_flags(COASTLINE_SWITCH);
	int iflag_draw_sph =   send_object_property_flags(SPHEREGRID_SWITCH);
	
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
	
	glut_menu_id->draw_rot_image_menu = glutCreateMenu(draw_rot_image_handler);
	glutAddMenuEntry("x-axis",ROTATE_X);
	glutAddMenuEntry("y-axis",ROTATE_Y);
	glutAddMenuEntry("z-axis",ROTATE_Z);
	return;
};

/* Create 1st level menu() */

static void make_1st_level_menu(){
	GLint menu_id;
	
	int iflag_draw_m = send_iflag_draw_mesh();
	int iflag_draw_p = send_iflag_draw_current_psf();
	int iflag_draw_f = send_iflag_draw_fline();
	int iflag_any_objects_on = iflag_draw_p + iflag_draw_m + iflag_draw_f;
	
	int nload_psf = send_num_loaded_PSF();
	
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
	}
	
	if( nload_psf > 0) glutAddSubMenu("Surface rendering", glut_menu_id->psf_root_menu);
	if( iflag_draw_f > 0) glutAddSubMenu("Field Lines", glut_menu_id->fline_root_menu);
	
	
	if ( iflag_draw_m > 0) {
		glutAddSubMenu("Color mode",         glut_menu_id->color_mode_menu);
	};
	
	if (iflag_any_objects_on > 0) {
		/*printf("polygon_id_menu \n");*/
		glutAddSubMenu("Object propaties",   glut_menu_id->polygon_id_menu);
		glutAddMenuEntry("Save image",         SAVE_SNAPSHOT);
	};
	
	if( iflag_draw_p+iflag_draw_f > 0){
		glutAddMenuEntry("Save evolution images", SAVE_EVOLUTION);
	};
	
	if (iflag_any_objects_on > 0) {
		glutAddMenuEntry("Save rotate images", SAVE_ROTATION);
		glutAddSubMenu("Rotate on Window",   glut_menu_id->draw_rot_image_menu);
	};
	
	glutAddMenuEntry("Quit",QUIT_SELECTED);
	glutAttachMenu(GLUT_LEFT_BUTTON);
	return;
};

/* Main routine for C */


void draw_mesh_kemo_glui(int iflag_streo_shutter, int iflag_dmesh) {
	int narg_glut = 0;
	char **arg_glut;
	
	/* Initialize arrays for viewer */
	allocate_single_kemoviwewer_struct(single_kemoview);
	set_to_stereo_shutter(iflag_streo_shutter);
	if(iflag_streo_shutter == SHUTTER_ON){
		set_to_iflag_anaglyph(ANAGLYPH_OFF);
	} else {
		set_to_iflag_anaglyph(ANAGLYPH_ON);
	};
	
	link_glut_menu_address();
	
	/*! Initializations with GLUT*/
	glutInit(&narg_glut, arg_glut);
	
	if(iflag_streo_shutter == SHUTTER_ON){
		glutInitDisplayMode(GLUT_RGBA|GLUT_DOUBLE|GLUT_DEPTH
				|GLUT_MULTISAMPLE|GLUT_STEREO);
		} else {
		glutInitDisplayMode(GLUT_RGBA|GLUT_DOUBLE|GLUT_DEPTH|GLUT_MULTISAMPLE);
	};
	
	set_to_pick_surface_command(pick_surf_command);
	
	/*! Create viewer window*/
    set_kemoview_retinamode(IZERO);
	set_kemoview_windowsize(NPIX_X, NPIX_Y);
	glutInitWindowSize(NPIX_X, NPIX_Y);
	winid = glutCreateWindow("Kemoviewer");
	set_main_window_id_glut(winid);
	/*glutEntryFunc(enter_leave);*/
	
	/*! Set the display callback  */
	
	glutDisplayFunc(display);
	
	if (!glutExtensionSupported("GL_ARB_texture_non_power_of_two")) 
		{printf("GL_ARB_texture_non_power_of_two is not Supported\n");};
	
	/*  initialize view_modifier, receiving the id for it's submenu  */
	reset_kemoviewer_to_init_angle();
	view_modifier_init();
	
	/* ! set the perspective and lighting */
	init_kemoview_background_color();
	kemoviewer_initial_lighting();
	
	
	/*! Create menu window*/
	menu_win = glutCreateSubWindow(winid,IZERO,IZERO,MENU_WIDTH,MENU_HEIGHT);
	/*glutEntryFunc(enter_leave);*/
	glutDisplayFunc(display_menu);
	
	draw_mesh_w_menu();
	
/*	GLUI_Master.set_glutIdleFunc(myGlutIdle);*/
	/*! set callback for GLUT*/
	glutMainLoop();
	return;
};

