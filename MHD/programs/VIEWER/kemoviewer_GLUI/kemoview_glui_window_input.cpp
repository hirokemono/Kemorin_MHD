/*
 *  kemoview_glui_window_input.cpp
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_glui_window_input.h"

static GLUI *glui_sub;

static GLUI_StaticText *staticText;
static GLUI_EditText   *editText;
static GLUI_EditText   *editText_min;
static GLUI_EditText   *editText_max;
static GLUI_EditText   *editText_data;
static GLUI_EditText   *editText_color;
static GLUI_EditText   *editText_opacity;
static GLUI_EditText   *editText_fline_thick;
static GLUI_RadioGroup *radiogroup_colormap;
static GLUI_RadioGroup *radiogroup_opacitymap;

static int obj_type = 1;
static float psf_color_min;
static float psf_color_max;
static float fline_color_min;
static float fline_color_max;
static float fline_thick;

static float value;
static float color;
static float opacity;

static int nline;
static int nloop;
static int num_inc;
static float coast_radius;
static float distance;
static float nodesize;
static float scaling;
static float thickness;
static int num_cloop;


static void close_panel(int val){
	draw_mesh_keep_menu();
	GLUI_Master.close_all();
	return;
}

/* Actions for valuable input */

static void input_psf_min_from_panel(int val){
	psf_color_min = editText_min->get_float_val();
	if(psf_color_max > psf_color_min){
		set_current_PSF_linear_colormap((double) psf_color_min, (double) psf_color_max);
		draw_mesh_keep_menu();
	};
	return;
}

static void input_psf_max_from_panel(int val){
	psf_color_max = editText_max->get_float_val();
	if(psf_color_max > psf_color_min){
		set_current_PSF_linear_colormap((double) psf_color_min, (double) psf_color_max);
		draw_mesh_keep_menu();
	};
	return;
}

static void select_colormap_point(int val){
	double dvalue, dcolor;
	int sel = radiogroup_colormap->get_int_val();
	send_current_PSF_color_table_items(sel, &dvalue, &dcolor);
	value = (float)dvalue;
	color = (float)dcolor;
	editText_data->set_float_val((float)dvalue);
	editText_color->set_float_val((float)dcolor);
	return;
}

static void select_opacitymap_point(int val){
	double dvalue, dopacity;
	int sel = radiogroup_opacitymap->get_int_val();
	send_current_PSF_opacity_table_items(sel, &dvalue, &dopacity);
	value = (float)dvalue;
	opacity = (float)dopacity;
	editText_data->set_float_val((float)dvalue);
	editText_opacity->set_float_val((float)dopacity);
	return;
}

static void input_psf_value_panel(int val){
	value = editText_data->get_float_val();
	return;
}
static void input_psf_color_panel(int val){
	color = editText_color->get_float_val();
	return;
}

static void input_psf_opacity_panel(int val){
	opacity = editText_opacity->get_float_val();
	return;
}

static void update_colormap_glui(int val){
	int sel = radiogroup_colormap->get_int_val();
	value = editText_data->get_float_val();
	color = editText_color->get_float_val();
	set_current_PSF_color_point(sel, (double) value, (double) color);
	close_panel(0);
	return;
}

static void add_colormap_glui(int val){
	value = editText_data->get_float_val();
	color = editText_color->get_float_val();
	add_current_PSF_color_idx_list((double) value, (double) color);
	close_panel(0);
	return;
}

static void delete_colormap_glui(int val){
	int sel = radiogroup_colormap->get_int_val();
	delete_current_PSF_color_idx_list(sel);
	close_panel(0);
	return;
}

static void update_opacitymap_glui(int val){
	int sel = radiogroup_opacitymap->get_int_val();
	value = editText_data->get_float_val();
	opacity = editText_opacity->get_float_val();
	set_current_PSF_opacity_point(sel, (double) value, (double) opacity);
	close_panel(0);
	return;
}

static void add_opacitymap_glui(int val){
	value = editText_data->get_float_val();
	opacity = editText_opacity->get_float_val();
	add_current_PSF_opacity_idx_list((double) value, (double) opacity);
	close_panel(0);
	return;
}

static void delete_opacitymap_glui(int val){
	int sel = radiogroup_opacitymap->get_int_val();
	delete_current_PSF_opacity_idx_list(sel);
	close_panel(0);
	return;
}


static void input_fline_min_from_panel(int val){
	fline_color_min = editText_min->get_float_val();
	if(fline_color_max > fline_color_min){
		input_fline_linear_colormap((double) fline_color_min, (double) fline_color_max);
		draw_mesh_keep_menu();
	};
	return;
}

static void input_fline_max_from_panel(int val){
	fline_color_max = editText_max->get_float_val();
	if(fline_color_max > fline_color_min){
		input_fline_linear_colormap((double) fline_color_min, (double) fline_color_max);
		draw_mesh_keep_menu();
	};
	return;
}

static void input_fline_thick_from_panel(int val){
	fline_thick = editText_fline_thick->get_float_val();
	set_to_fline_thickness((double) fline_thick);
	GLUI_Master.close_all();
	draw_mesh_keep_menu();
	return;
}

static void input_psf_opacity_from_panel(int val){
	opacity = editText_opacity->get_float_val();
	set_current_PSF_constant_opacity((double) opacity);
	GLUI_Master.close_all();
	draw_mesh_keep_menu();
	return;
}

static void input_domain_opacity_from_panel(int val){
	opacity = editText_opacity->get_float_val();
	set_to_domain_surface_opacity((double) opacity);
	GLUI_Master.close_all();
	draw_mesh_keep_menu();
	return;
}

static void input_egrp_opacity_from_panel(int val){
	opacity = editText_opacity->get_float_val();
	set_to_ele_surface_opacity((double) opacity);
	GLUI_Master.close_all();
	draw_mesh_keep_menu();
	return;
}

static void input_sgrp_opacity_from_panel(int val){
	opacity = editText_opacity->get_float_val();
	set_to_surf_surface_opacity((double) opacity);
	GLUI_Master.close_all();
	draw_mesh_keep_menu();
	return;
}

static void input_num_isoline_from_panel(int val){
	nline = editText->get_int_val();
	if(nline > 0) set_current_n_isoline(nline);
	GLUI_Master.close_all();
	draw_mesh_keep_menu();
	return;
}

static void input_vector_increment_from_panel(int val){
	num_inc = editText->get_int_val();
	if(num_inc > 0) set_current_increment_vect(num_inc);
	GLUI_Master.close_all();
	draw_mesh_keep_menu();
	return;
}

static void input_psf_vector_scale_panel(int val){
	scaling = editText->get_float_val();
	set_current_scale_vect((double) scaling);
	GLUI_Master.close_all();
	draw_mesh_keep_menu();
	return;
}

static void input_psf_vector_thickness_panel(int val){
	thickness = editText->get_float_val();
	set_current_vector_thick((double) thickness);
	GLUI_Master.close_all();
	draw_mesh_keep_menu();
	return;
}

static void input_coast_radius_from_panel(int val){
	coast_radius = editText->get_float_val();
	set_to_coastline_radius((double) coast_radius);
	GLUI_Master.close_all();
	draw_mesh_keep_menu();
	return;
}

static void input_domain_distance_from_panel(int val){
	distance = editText->get_float_val();
	set_to_dist_domains((double) distance);
	GLUI_Master.close_all();
	draw_mesh_keep_menu();
	return;
}

static void input_num_colorloop_from_panel(int val){
	nloop = editText->get_int_val();
	if(nloop > 0) set_to_num_of_color_loop(nloop);
	GLUI_Master.close_all();
	draw_mesh_keep_menu();
	return;
}

static void input_node_size_from_panel(int val){
	nodesize = editText->get_float_val();
	set_to_node_diam((double) nodesize);
	GLUI_Master.close_all();
	draw_mesh_keep_menu();
	return;
}


/* Panels for valuable input */

void set_psf_range_by_glui(int winid){
	char psf_range_txt[1024];
	
	int ifield = send_draw_field_current_psf();
	int icomp = send_draw_component_current_psf();
	psf_color_min = (float) send_current_PSF_color_table_min();
	psf_color_max = (float) send_current_PSF_color_table_max();
	sprintf(psf_range_txt,"Range: %3.2e ... %3.2e",(float) send_current_psf_data_min(icomp),
			(float) send_current_psf_data_max(icomp) );
	glui_sub = GLUI_Master.create_glui("PSF range", 0, 100, 100);
	staticText = new GLUI_StaticText( glui_sub, psf_range_txt);
	editText_max = new GLUI_EditText( glui_sub, "Maximum: ", GLUI_EDITTEXT_FLOAT,
									 &psf_color_max, -1, input_psf_max_from_panel );
	editText_min = new GLUI_EditText( glui_sub, "Maximum: " , GLUI_EDITTEXT_FLOAT,
									 &psf_color_min, -1, input_psf_min_from_panel );
	glui_sub->add_button("OK", 0, close_panel);
	glui_sub->set_main_gfx_window(winid);
	return;
}

void set_fline_range_by_glui(int winid){
	char fline_range_txt[1024];
	
	int ifield = send_if_draw_fline();
	int icomp = send_icomp_draw_fline();
	fline_color_min = (float) kemoview_get_fline_min_color();
	fline_color_max = (float) kemoview_get_fline_max_color();
	sprintf(fline_range_txt,"Range: %3.2e ... %3.2e",(float) send_fline_data_min(icomp),
			(float) send_fline_data_max(icomp) );
	
	glui_sub = GLUI_Master.create_glui("Field line color", 0, 100, 100);
	staticText = new GLUI_StaticText( glui_sub, fline_range_txt);
	editText_max = new GLUI_EditText( glui_sub, "Maximum: ", GLUI_EDITTEXT_FLOAT,
									 &fline_color_max, -1, input_fline_max_from_panel );
	editText_min = new GLUI_EditText( glui_sub, "Minimum: ", GLUI_EDITTEXT_FLOAT,
									 &fline_color_min, -1, input_fline_min_from_panel );
	glui_sub->add_button("OK", 0, close_panel);
	glui_sub->set_main_gfx_window(winid);
	return;
}

void set_fline_thick_glui(int winid){
	fline_thick = (float) send_fline_thickness();
	
	glui_sub = GLUI_Master.create_glui("Set fieldline thickness", 0, 100, 100);
	editText_fline_thick = new GLUI_EditText( glui_sub, "Thickness", GLUI_EDITTEXT_FLOAT,
										 &fline_thick, -1, input_fline_thick_from_panel );
	glui_sub->set_main_gfx_window(winid);
	return;
}

void set_psf_opacity_by_glui(int winid){
	opacity = (float) send_current_PSF_maximum_opacity();
	
	glui_sub = GLUI_Master.create_glui("Set PSF Parameter", 0, 100, 100);
	editText_opacity = new GLUI_EditText( glui_sub, "Opacity", GLUI_EDITTEXT_FLOAT,
										 &opacity, -1, input_psf_opacity_from_panel );
	glui_sub->set_main_gfx_window(winid);
	return;
}

void set_domain_opacity_by_glui(int winid){
	opacity = (float) send_domain_surface_opacity();
	
	glui_sub = GLUI_Master.create_glui("Domain Parameter", 0, 100, 100);
	editText_opacity = new GLUI_EditText( glui_sub, "Opacity: ", GLUI_EDITTEXT_FLOAT,
										 &opacity, -1, input_domain_opacity_from_panel );
	glui_sub->set_main_gfx_window(winid);
	return;
}

void set_ele_group_opacity_by_glui(int winid){
	opacity = (float) send_ele_surface_opacity();
	
	glui_sub = GLUI_Master.create_glui("Element group Parameter", 0, 100, 100);
	editText_opacity = new GLUI_EditText( glui_sub, "Opacity: ", GLUI_EDITTEXT_FLOAT,
										 &opacity, -1, input_egrp_opacity_from_panel );
	glui_sub->set_main_gfx_window(winid);
	return;
}

void set_surf_group_opacity_by_glui(int winid){
	opacity = (float) send_surf_surface_opacity();
	
	glui_sub = GLUI_Master.create_glui("Surface group Parameter", 0, 100, 100);
	editText_opacity = new GLUI_EditText( glui_sub, "Opacity: ", GLUI_EDITTEXT_FLOAT,
										 &opacity, -1, input_sgrp_opacity_from_panel );
	glui_sub->set_main_gfx_window(winid);
	return;
}


void set_num_isoline_from_glui(int winid){
	nline = send_current_num_isoline();
	glui_sub = GLUI_Master.create_glui("Set PSF Parameter", 0, 100, 100);
	editText = new GLUI_EditText( glui_sub, "num. of isoline: ",
								 GLUI_EDITTEXT_INT, &nline, -1, input_num_isoline_from_panel );
	glui_sub->set_main_gfx_window(winid);
	return;
}

void set_psf_vect_increment_glui(int winid){
	num_inc = send_current_increment_vect();
	
	glui_sub = GLUI_Master.create_glui("Set vector increment", 0, 100, 100);
	editText = new GLUI_EditText( glui_sub, "vector increment: ", GLUI_EDITTEXT_INT,
								 &num_inc, -1, input_vector_increment_from_panel );
	glui_sub->set_main_gfx_window(winid);
	return;
}

void set_psf_vector_scale_by_glui(int winid){
	scaling = (float) send_current_scale_vect();
	
	glui_sub = GLUI_Master.create_glui("Set unit length of vector", 0, 100, 100);
	editText = new GLUI_EditText( glui_sub, "Vector scaling: ", GLUI_EDITTEXT_INT,
								 &scaling, -1, input_psf_vector_scale_panel );
	set_current_scale_vect(nodesize);
	glui_sub->set_main_gfx_window(winid);
	return;
}

void set_psf_vector_thick_by_glui(int winid){
	thickness = (float) send_current_vector_thick();
	
	glui_sub = GLUI_Master.create_glui("Set vector thickness", 0, 100, 100);
	editText = new GLUI_EditText( glui_sub, "Vector thickness", GLUI_EDITTEXT_FLOAT,
								 &thickness, -1, input_psf_vector_thickness_panel );
	set_current_vector_thick(thickness);
	glui_sub->set_main_gfx_window(winid);
	return;
}

void set_coastline_radius_glui(int winid){
	coast_radius = (float) send_coastline_radius();
	
	glui_sub = GLUI_Master.create_glui("Domain Parameter", 0, 100, 100);
	editText = new GLUI_EditText( glui_sub, "Coastline radius: ", GLUI_EDITTEXT_FLOAT,
								 &coast_radius, -1, input_coast_radius_from_panel );
	glui_sub->set_main_gfx_window(winid);
	return;
}

void set_domain_distance_by_glui(int winid){
	distance = (float) send_dist_domains();
	
	glui_sub = GLUI_Master.create_glui("Domain Parameter", 0, 100, 100);
	editText = new GLUI_EditText( glui_sub, "Object distance: ", GLUI_EDITTEXT_FLOAT,
								 &distance, -1, input_domain_distance_from_panel );
	glui_sub->set_main_gfx_window(winid);
	return;
}

void set_num_color_loop_by_glui(int winid){
	nloop = send_num_of_color_loop();
	
	glui_sub = GLUI_Master.create_glui("Domain Parameter", 0, 100, 100);
	editText = new GLUI_EditText( glui_sub, "Num of color: ", GLUI_EDITTEXT_INT,
								 &nloop, -1, input_num_colorloop_from_panel );
	glui_sub->set_main_gfx_window(winid);
	return;
}

void set_node_size_by_glui(int winid){
	nodesize = (float) send_node_diam();
	
	glui_sub = GLUI_Master.create_glui("Domain Parameter", 0, 100, 100);
	editText = new GLUI_EditText( glui_sub, "Node size: ", GLUI_EDITTEXT_FLOAT,
								 &nodesize, -1, input_node_size_from_panel );
	glui_sub->set_main_gfx_window(winid);
	return;
}


void edit_psf_colormap_by_glui(int winid){
	GLUI_Panel *color_panel;
	int i, nloop;
	double dvalue, dcolor;
	char tmp_menu[1024];
	
	nloop = send_current_PSF_color_table_num();
	
	glui_sub = GLUI_Master.create_glui("Color map editor", 0, 100, 100);
	color_panel = new GLUI_Panel(glui_sub, "Colormap (data, color)");
	
	radiogroup_colormap = new GLUI_RadioGroup(color_panel, &obj_type, -1, select_colormap_point);
	for(i = 0; i < nloop; i++) {
		send_current_PSF_color_table_items(i, &dvalue, &dcolor);
		sprintf(tmp_menu, "%3.2e	|	%.2f", (float) dvalue, (float) dcolor);
		new GLUI_RadioButton(radiogroup_colormap, tmp_menu);
	};
	
	send_current_PSF_color_table_items(radiogroup_colormap->get_int_val(), &dvalue, &dcolor);
	value = (float)dvalue;
	color = (float)dcolor;
	editText_data = new GLUI_EditText( glui_sub, "data: ", GLUI_EDITTEXT_FLOAT,
									 &value, -1, input_psf_value_panel );
	editText_color = new GLUI_EditText( glui_sub, "color: " , GLUI_EDITTEXT_FLOAT,
									 &color, -1, input_psf_color_panel );
	glui_sub->add_button("Update selected", 0, update_colormap_glui);
	glui_sub->add_button("Add feature point", 0, add_colormap_glui);
	glui_sub->add_button("Delete selected", 0, delete_colormap_glui);
	glui_sub->add_button("Cancel", 0, close_panel);
	return;
};
void edit_psf_opacitymap_by_glui(int winid){
	GLUI_Panel *opacity_panel;
	int i, nloop;
    double dvalue, dopacity;
	char tmp_menu[1024];
	
	nloop = send_current_PSF_opacity_table_num();
	
	glui_sub = GLUI_Master.create_glui("Opacity map editor", 0, 100, 100);
	opacity_panel = new GLUI_Panel(glui_sub, "Opacity map (data, color)");
	
	radiogroup_opacitymap = new GLUI_RadioGroup(opacity_panel, &obj_type, -1, select_opacitymap_point);
	for(i = 0; i < nloop; i++) {
		send_current_PSF_opacity_table_items(i, &dvalue, &dopacity);
		sprintf(tmp_menu, "%3.2e	|	%.2f", (float) dvalue, (float) dopacity);
		new GLUI_RadioButton(radiogroup_opacitymap, tmp_menu);
	};
	
	
	send_current_PSF_opacity_table_items(radiogroup_opacitymap->get_int_val(), &dvalue, &dopacity);
	value = (float)dvalue;
	opacity = (float)dopacity;
	
	editText_data = new GLUI_EditText( glui_sub, "data: ", GLUI_EDITTEXT_FLOAT,
									 &value, -1, input_psf_value_panel );
	editText_opacity = new GLUI_EditText( glui_sub, "opacity: " , GLUI_EDITTEXT_FLOAT,
									 &opacity, -1, input_psf_opacity_panel );
	glui_sub->add_button("Update selected", 0, update_opacitymap_glui);
	glui_sub->add_button("Add feature point", 0, add_opacitymap_glui);
	glui_sub->add_button("Delete selected", 0, delete_opacitymap_glui);
	glui_sub->add_button("Cancel", 0, close_panel);
	return;
};
