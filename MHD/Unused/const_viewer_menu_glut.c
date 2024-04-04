
/* const_viewer_menu_glut.c */

#include "const_viewer_menu_glut.h"

void glut_drawing_select() {
	int iflag_shade = kemoview_get_object_property_flags(kemo_sgl, SHADING_SWITCH);
	int iflag_polygon = kemoview_get_object_property_flags(kemo_sgl, POLYGON_SWITCH);
	
	if(iflag_shade == FLAT_SHADE ){
		glutAddMenuEntry("flat => smooth",SHADING_SWITCH);
	}
	else{
		glutAddMenuEntry("smooth => flat",SHADING_SWITCH);
	};
	if(iflag_polygon== NORMAL_POLYGON ){
		glutAddMenuEntry("normal => reverse",POLYGON_SWITCH);
	}
	else{
		glutAddMenuEntry("reverse => normal",POLYGON_SWITCH);
	};
	glutAddMenuEntry("size of node", SET_NODE_SIZE);
	
	return;
}


void glut_current_PSF_select() {
	int i, istep;
	int id_current;
	char title[LENGTHBUF];
    struct kv_string *stripped_filehead;
	
	id_current = kemoview_get_curent_PSF_ID();
	for (i=0; i< kemoview_get_PSF_max_loaded(); i++) {
		if(i != id_current && kemoview_get_PSF_loaded_flag(kemo_sgl, i) > 0){
			kemoview_set_current_PSF(i);
            stripped_filehead = kemoview_alloc_kvstring();
			istep = kemoview_get_PSF_file_prefix(kemo_sgl, stripped_filehead);
			sprintf(title, "%d: %s", i, stripped_filehead->string);
            kemoview_free_kvstring(stripped_filehead);
			glutAddMenuEntry(title,i);
		}
	};
    kemoview_set_current_PSF(id_current);
	return;
}

void glut_PSF_field_select() {
	int i;
	int num_field = kemoview_get_PSF_num_field();
	int if_psf = kemoview_get_PSF_field_id();
    struct kv_string *colorname = kemoview_alloc_kvstring();
	
	for ( i = 0; i< num_field; i++) {
		if( i != if_psf){
			kemoview_get_PSF_field_name(kemo_sgl, colorname, i);
			glutAddMenuEntry(colorname->string,i);
		}
	};
    kemoview_free_kvstring(colorname);
	return;
}

void glut_fline_color_select(){
	int i;
	int num_field = kemoview_get_fline_color_num_field();
	int if_fline = kemoview_get_fline_color_field();
    struct kv_string *colorname = kemoview_alloc_kvstring();
	
	for ( i = 0; i< num_field; i++) {
		if( i != if_fline) {
			kemoview_get_PSF_field_name(kemo_sgl, colorname, i);
			glutAddMenuEntry(colorname->string,i);
		}
	};
    kemoview_free_kvstring(colorname);

	return;
}

void set_PSF_component_name(int ncomp, int icomp, char *comp_name) {
    int id_coord =    kemoview_get_PSF_coordinate_id();
	
	if(id_coord == 1){
		if(ncomp == 3){
			if(icomp == 0) sprintf(comp_name, "R");
			if(icomp == 1) sprintf(comp_name, "Theta");
			if(icomp == 2) sprintf(comp_name, "Phi");
		} else if(ncomp == 6){
			if(icomp == 0) sprintf(comp_name, "RR");
			if(icomp == 1) sprintf(comp_name, "R-Theta");
			if(icomp == 2) sprintf(comp_name, "R-Phi");
			if(icomp == 3) sprintf(comp_name, "Theta-Theta");
			if(icomp == 4) sprintf(comp_name, "Theta-Phi");
			if(icomp == 5) sprintf(comp_name, "Phi-Phi");
		};
	} else if(id_coord == 2){
		if(ncomp == 3){
			if(icomp == 0) sprintf(comp_name, "S");
			if(icomp == 1) sprintf(comp_name, "Phi");
			if(icomp == 2) sprintf(comp_name, "Z");
		} else if(ncomp == 6){
			if(icomp == 0) sprintf(comp_name, "SS");
			if(icomp == 1) sprintf(comp_name, "S-Phi");
			if(icomp == 2) sprintf(comp_name, "SZ");
			if(icomp == 3) sprintf(comp_name, "Phi-Phi");
			if(icomp == 4) sprintf(comp_name, "Phi-Z");
			if(icomp == 5) sprintf(comp_name, "ZZ");
		};
	} else {
		if(ncomp == 3){
			if(icomp == 0) sprintf(comp_name, "X");
			if(icomp == 1) sprintf(comp_name, "Y");
			if(icomp == 2) sprintf(comp_name, "Z");
		} else if(ncomp == 6){
			if(icomp == 0) sprintf(comp_name, "XX");
			if(icomp == 1) sprintf(comp_name, "XY");
			if(icomp == 2) sprintf(comp_name, "XZ");
			if(icomp == 3) sprintf(comp_name, "YY");
			if(icomp == 4) sprintf(comp_name, "YZ");
			if(icomp == 5) sprintf(comp_name, "ZZ");
		};
	};
	
	if(icomp == ncomp) sprintf(comp_name,"magnitude");
	
	return;
};


void glut_PSF_comps_select() {
	char tmp_menu[1024];
	int i;
	int if_psf =   kemoview_get_PSF_field_id();
	int ndir = (int) kemoview_get_PSF_num_component(kemo_sgl, if_psf);
	
	for(i=0;i<ndir;i++){
		if(kemoview_get_PSF_component_id() != i){
			set_PSF_component_name(ndir, i, tmp_menu);
			glutAddMenuEntry(tmp_menu,i);
		}
	}
	glutAddMenuEntry("magnitude \n",ndir);
	
	return;
};

void glut_PSF_patchcolor_select(){
	int iflag_p_color = kemoview_get_PSF_patch_color_mode();

	if (iflag_p_color != RAINBOW_SURFACE) {
		glutAddMenuEntry("Rainbow surface", RAINBOW_PSF_SURF);
	};
	if (iflag_p_color != WHITE_SURFACE) {
		glutAddMenuEntry("White surface",   WHITE_PSF_SURF);
	};
    if (iflag_p_color != SINGLE_COLOR) {
        glutAddMenuEntry("Single color",    SGL_COLOR_PSF_SURF);
    };
	if (iflag_p_color != TEXTURED_SURFACE) {
		glutAddMenuEntry("Textured surface",TEXTURE_PSF_SURF);
	};

	return;
};

void glut_PSF_linecolor_select(){
	int iflag_l_color = kemoview_get_PSF_isoline_color_mode();

	if (iflag_l_color != RAINBOW_LINE) {
		glutAddMenuEntry("Rainbow lines", RAINBOW_PSF_LINE);
	};
	if (iflag_l_color != BLACK_PSF_LINE) {
		glutAddMenuEntry("Black lines", RAINBOW_PSF_LINE);
	};
	if (iflag_l_color != WHITE_PSF_LINE) {
		glutAddMenuEntry("White lines", WHITE_PSF_LINE);
	};
	
	return;
};

void glut_PSF_colormode_select(){
    int iflag_cmap_mode;
    iflag_cmap_mode = kemoview_get_PSF_color_mode();
    
    if (iflag_cmap_mode != RAINBOW_MODE) {
        glutAddMenuEntry("Rainbow", RAINBOW_MODE);
    };
    if (iflag_cmap_mode != RED_BLUE_MODE) {
        glutAddMenuEntry("Blue to Red", RED_BLUE_MODE);
    };
    if (iflag_cmap_mode != GRAYSCALE_MODE) {
        glutAddMenuEntry("Grayscale", GRAYSCALE_MODE);
    };
    if (iflag_cmap_mode != SYM_GRAY_MODE) {
        glutAddMenuEntry("Symmetric Grayscale", SYM_GRAY_MODE);
    };
    
    return;
}


void glut_fline_color_comp_select() {
	int i;
	char tmp_menu[1024];
	int if_fline = kemoview_get_fline_color_field();
	int ic_fline = kemoview_get_fline_color_component();
	int ndir = kemoview_get_fline_color_num_comps(kemo_sgl, if_fline);
	
	for ( i = 0; i< ndir; i++) {
		if( i != ic_fline){
			sprintf(tmp_menu, "component: %d", (i+1) ); 
			/*puts(tmp_menu); */
			glutAddMenuEntry(tmp_menu,i);
		};
	};
	glutAddMenuEntry("magnitude \n",ndir);
	
	return;
};

void glut_PSF_draw_menu(){
	int iflag_solid =       kemoview_get_PSF_draw_flags(kemo_sgl, PSFSOLID_TOGGLE);
	int iflag_grid =        kemoview_get_PSF_draw_flags(kemo_sgl, PSFGRID_TOGGLE);
	int iflag_zero =        kemoview_get_PSF_draw_flags(kemo_sgl, ZEROGRID_TOGGLE);
	int iflag_cbar =        kemoview_get_PSF_draw_flags(kemo_sgl, COLORBAR_TOGGLE);
	int iflag_vect =        kemoview_get_PSF_draw_flags(kemo_sgl, PSFVECT_TOGGLE);
	int iflag_PSF_polygon = kemoview_get_PSF_draw_flags(kemo_sgl, PSF_POLYGON_SWITCH);
    int iflag_PSF_tanvec =  kemoview_get_PSF_draw_flags(kemo_sgl, PSFTANVEC_TOGGLE);
	int if_psf =            kemoview_get_PSF_field_id();
	int ncomp =       (int) kemoview_get_PSF_num_component(kemo_sgl, if_psf);
	
	if(iflag_solid != IZERO ){
		glutAddMenuEntry("hide PSF surface",PSFSOLID_TOGGLE);
	}
	else{
		glutAddMenuEntry("show PSF surface",PSFSOLID_TOGGLE);
	};
	
	if(iflag_grid != IZERO ){
		glutAddMenuEntry("hide PSF isolines",PSFGRID_TOGGLE);
	}
	else{
		glutAddMenuEntry("show PSF isolines",PSFGRID_TOGGLE);
	};
	
	if(iflag_zero != IZERO ){
		glutAddMenuEntry("hide zero lines",ZEROGRID_TOGGLE);
	}
	else{
		glutAddMenuEntry("show zero lines",ZEROGRID_TOGGLE);
	};
	
	if(iflag_cbar != IZERO ){
		glutAddMenuEntry("hide colorbar",COLORBAR_TOGGLE);
	}
	else{
		glutAddMenuEntry("show colorbar",COLORBAR_TOGGLE);
	};
	
	if(ncomp == 3){
		if(iflag_vect != IZERO ){
			glutAddMenuEntry("hide PSF Vector",PSFVECT_TOGGLE);
		}else{
			glutAddMenuEntry("show PSF Vector",PSFVECT_TOGGLE);
		};
	};

	if(iflag_solid != IZERO ){
		if(iflag_PSF_polygon== NORMAL_POLYGON ){
			glutAddMenuEntry("normal => reverse",PSF_POLYGON_SWITCH);
		}
		else{
			glutAddMenuEntry("reverse => normal",PSF_POLYGON_SWITCH);
		};
	};
    
	if(iflag_solid != IZERO ){
		if(iflag_PSF_polygon== NORMAL_POLYGON ){
			glutAddMenuEntry("full => tangential",PSFTANVEC_TOGGLE);
		}
		else{
			glutAddMenuEntry("tangential => full",PSFTANVEC_TOGGLE);
		};
	};
	return;
}
	
void glut_PSF_range_menu(){
	int if_psf =        kemoview_get_PSF_field_id();
	int ncomp =   (int) kemoview_get_PSF_num_component(kemo_sgl, if_psf);
	int iflag_vect =    kemoview_get_PSF_draw_flags(kemo_sgl, PSFVECT_TOGGLE);
	int iflag_v_color = kemoview_get_PSF_vector_color_mode();
/*	int iflag_refv =  kemoview_get_PSF_draw_refv();*/

	glutAddMenuEntry("Set range",              ISET_RANGE);
	glutAddMenuEntry("Set number of isolines", ISET_NLINE);
	glutAddMenuEntry("Set Opacity",            ISET_PSF_OPACITY);
	
	
	if(iflag_vect != IZERO && ncomp == 3){
		/*
		if(iflag_refv != IZERO ){
			glutAddMenuEntry("hide Vector Reference",PSFREFV_TOGGLE);
		}
		else{
			glutAddMenuEntry("show Vector Reference",PSFREFV_TOGGLE);
		};
		*/
		if (iflag_v_color == RAINBOW_SURFACE) {
			glutAddMenuEntry("Black Vector",          WHITE_PSF_VECT);
		}else{
			glutAddMenuEntry("Rainbow Vector",        RAINBOW_PSF_VECT);
		};
	
		glutAddMenuEntry("Set Increment for Vector", ISET_PSF_VEC_INC);
		glutAddMenuEntry("Set Reference Vector",     ISET_PSF_REFVECT);
		glutAddMenuEntry("Set Vector Thickness",     ISET_PSF_V_THICK);
	};
	
	return;
};


static void glut_add_each_grp_menu_item(int i_item, int iflag_draw, char *grp_name){
	char tmp_menu[80];
	
	if(iflag_draw != 0) {
		sprintf(tmp_menu, "Hide %s", grp_name);
	} else{
		sprintf(tmp_menu, "Show %s", grp_name);
	};
	glutAddMenuEntry(tmp_menu,i_item);
}

void glut_nod_grp_menu_item(){
	int i;
	int iflag_draw;
    struct kv_string *groupname;
	int ngrp = kemoview_get_num_node_grp();
	
	glutAddMenuEntry("Hide all",ngrp+1);
	for (i = 0; i < ngrp; i++){
		iflag_draw = kemoview_get_draw_nodgrp_node(i);
        groupname = kemoview_get_group_name(kemo_sgl, NODE_GRP_FLAG, i);
		glut_add_each_grp_menu_item(i, iflag_draw, groupname->string);
        kemoview_free_kvstring(groupname);
	};
	glutAddMenuEntry("Show all",ngrp);
}

void glut_surf_grp_patch_menu(){
	int i, iflag_draw;
    struct kv_string *groupname;
	int ngrp = kemoview_get_num_surf_grp();
	
	glutAddMenuEntry("Hide all",ngrp+1);
	for (i = 0; i < ngrp; i++){
		iflag_draw = kemoview_get_draw_surfgrp_patch(i);
        groupname = kemoview_get_group_name(kemo_sgl, SURF_GRP_FLAG, i);
        
		glut_add_each_grp_menu_item(i, iflag_draw, groupname->string);
        kemoview_free_kvstring(groupname);
	};
	glutAddMenuEntry("Show all",ngrp);
	return;
}
void glut_surf_grp_edge_menu(){
	int i, iflag_draw;
    struct kv_string *groupname;
	int ngrp = kemoview_get_num_surf_grp();
	
	glutAddMenuEntry("Hide all",ngrp+1);
	for (i = 0; i < ngrp; i++){
		iflag_draw = kemoview_get_draw_surfgrp_grid(i);
        groupname = kemoview_get_group_name(kemo_sgl, SURF_GRP_FLAG, i);
		glut_add_each_grp_menu_item(i, iflag_draw, groupname->string);
        kemoview_free_kvstring(groupname);
	};
	glutAddMenuEntry("Show all",ngrp);
	return;
}
void glut_surf_grp_node_menu(){
	int i, iflag_draw;
    struct kv_string *groupname;
	int ngrp = kemoview_get_num_surf_grp();
	
	glutAddMenuEntry("Hide all",ngrp+1);
	for (i = 0; i < ngrp; i++){
		iflag_draw = kemoview_get_draw_surfgrp_node(i);
        groupname = kemoview_get_group_name(kemo_sgl, SURF_GRP_FLAG, i);
		glut_add_each_grp_menu_item(i, iflag_draw, groupname->string);
        kemoview_free_kvstring(groupname);
	};
	glutAddMenuEntry("Show all",ngrp);
	return;
}

void glut_ele_grp_patch_menu(){
	int i, iflag_draw;
    struct kv_string *groupname;
	int ngrp = kemoview_get_num_ele_grp();
	
	glutAddMenuEntry("Hide all",ngrp+1);
	for (i = 0; i < ngrp; i++){
		iflag_draw = kemoview_get_draw_elegrp_patch(i);
        groupname = kemoview_get_group_name(kemo_sgl, ELEM_GRP_FLAG, i);
		glut_add_each_grp_menu_item(i, iflag_draw, groupname->string);
        kemoview_free_kvstring(groupname);
	};
	glutAddMenuEntry("Show all",ngrp);
	return;
}
void glut_ele_grp_edge_menu(){
	int i, iflag_draw;
    struct kv_string *groupname;
	int ngrp = kemoview_get_num_ele_grp();
	
	glutAddMenuEntry("Hide all",ngrp+1);
	for (i = 0; i < ngrp; i++){
		iflag_draw = kemoview_get_draw_elegrp_grid(i);
        groupname = kemoview_get_group_name(kemo_sgl, ELEM_GRP_FLAG, i);
		glut_add_each_grp_menu_item(i, iflag_draw, groupname->string);
        kemoview_free_kvstring(groupname);
	};
	glutAddMenuEntry("Show all",ngrp);
	return;
}
void glut_ele_grp_node_menu(){
	int i, iflag_draw;
    struct kv_string *groupname;
	int ngrp = kemoview_get_num_ele_grp();
	
	glutAddMenuEntry("Hide all",ngrp+1);
	for (i = 0; i < ngrp; i++){
        groupname = kemoview_get_group_name(kemo_sgl, ELEM_GRP_FLAG, i);
		iflag_draw = kemoview_get_draw_elegrp_node(i);
		glut_add_each_grp_menu_item(i, iflag_draw, groupname->string);
        kemoview_free_kvstring(groupname);
	};
	glutAddMenuEntry("Show all",ngrp);
	return;
}

void glut_viewtype_menu(){
	int iflag_view = kemoview_get_view_type_flag(kemo_sgl);
	
	if( iflag_view != VIEW_3D){
		glutAddMenuEntry("3D-Viewer",VIEW_3D);
	};
	if( iflag_view != VIEW_STEREO){
		glutAddMenuEntry("Stereo-Viewer",VIEW_STEREO);
	};
	if( iflag_view != VIEW_MAP ){
		glutAddMenuEntry("Map-Viewer",VIEW_MAP);
	};
	if( iflag_view != VIEW_XY ){
		glutAddMenuEntry("XY-Viewer",VIEW_XY);
	};
	if( iflag_view != VIEW_XZ ){
		glutAddMenuEntry("XZ-Viewer",VIEW_XZ);
	};
	if( iflag_view != VIEW_YZ ){
		glutAddMenuEntry("YZ-Viewer",VIEW_YZ);
	};
	glutAddMenuEntry("reset to initial view",RESET);
	
	return;
}

void glut_mesh_display_menu(){
	int iflag_nod =   kemoview_get_draw_mesh_node(0);
	int iflag_grid =  kemoview_get_draw_mesh_grid(0);
	int iflag_solid = kemoview_get_draw_mesh_patch(0);
	
	if( iflag_solid != IZERO ){
		glutAddMenuEntry("Hide solid surface",SURFSOLID_TOGGLE);
	}
	else{
		glutAddMenuEntry("Show solid surface",SURFSOLID_TOGGLE);
	};
	
	if( iflag_grid != IZERO ){
		glutAddMenuEntry("Hide wireframe",SURFGRID_TOGGLE);
	}
	else{
		glutAddMenuEntry("Show wireframe",SURFGRID_TOGGLE);
	};
	
	if( iflag_nod != IZERO ){
		glutAddMenuEntry("Hide nodes",SURFNOD_TOGGLE);
	}
	else{
		glutAddMenuEntry("Show nodes",SURFNOD_TOGGLE);
	};
	
	return;
};

void glut_color_mode_menu_item(){
	glutAddMenuEntry("Grayscale",     GRAYSCALE); 
	glutAddMenuEntry("Rainbow color", RAINBOW_COLOR); 
	glutAddMenuEntry("Set number of colors", SET_NUM_COLORS); 
	
	return;
}

void glut_draw_axis_menu_item(int iflag_draw_axis){
	if(iflag_draw_axis != 0) {
		glutAddMenuEntry("Hide axis", AXIS_TOGGLE);
	}
	else{
		glutAddMenuEntry("Show axis", AXIS_TOGGLE);
	};
	return;
}

void glut_draw_coast_menu_item(int iflag_draw_coast){
	if(iflag_draw_coast != 0) {
		glutAddMenuEntry("Hide coastline", COASTLINE_SWITCH);
	}
	else{
		glutAddMenuEntry("Show coastline", COASTLINE_SWITCH);
	};
	return;
}

void glut_draw_sph_grid_menu_item(int iflag_draw_sph){
	if(iflag_draw_sph != 0) {
		glutAddMenuEntry("Hide sphere grid", SPHEREGRID_SWITCH);
	}
	else{
		glutAddMenuEntry("Show sphere grid", SPHEREGRID_SWITCH);
	};
	return;
}


void glut_save_image_menu_item(char *file_head){
	char tmp_menu[80];
	
	sprintf(tmp_menu, "%s.png",file_head); 
	glutAddMenuEntry(tmp_menu,SAVE_PNG);
	
	sprintf(tmp_menu, "%s.bmp",file_head); 
	glutAddMenuEntry(tmp_menu,SAVE_BMP);
	
	return;
}

void glut_color_menu_item(){
	glutAddMenuEntry("white",          WHITE_SURFACE);
	glutAddMenuEntry("green",          GREEN_SURFACE);
	glutAddMenuEntry("color by domain",DOMAIN_COLOR);
}

void glut_line_color_menu_item(){
	glutAddMenuEntry("black",          BLACK_LINE);
	glutAddMenuEntry("green",          GREEN_SURFACE);
	glutAddMenuEntry("color by domain",DOMAIN_COLOR);
}

void glut_grp_color_menu_item(){
	glut_color_menu_item();
	glutAddMenuEntry("color by group", GROUP_COLOR);
}
void glut_surf_color_menu_item(){
	glut_grp_color_menu_item();
	glutAddMenuEntry("set opacity", SET_OPACITY);
}

void glut_fline_col_type_menu(){
	int icolor = kemoview_get_fline_colormode();
	if (icolor != RAINBOW_LINE)
		glutAddMenuEntry("Rainbow lines", RAINBOW_LINE);
	if (icolor != TWO_COLOR_LINE)
		glutAddMenuEntry("Two colord linees", TWO_COLOR_LINE);
	if (icolor != TWO_GRAY_LINE)
		glutAddMenuEntry("Two grayscale linees", TWO_GRAY_LINE);
	if (icolor != BLACK_LINE)
		glutAddMenuEntry("Black lines",   BLACK_LINE);
	return;
};

