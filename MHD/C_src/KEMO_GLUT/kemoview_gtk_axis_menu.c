/*
 *  kemoview_gtk_axis_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_axis_menu.h"

static void draw_axis_switch_CB(GObject *switch_bar, GParamSpec *pspec, gpointer data){
	int toggle = kemoview_toggle_object_properties(AXIS_TOGGLE);
	return;
};
static void draw_coastline_switch_CB(GObject *switch_bar, GParamSpec *pspec, gpointer data){
	int toggle = kemoview_toggle_object_properties(COASTLINE_SWITCH);
	return;
};
static void draw_sph_grid_switch_CB(GObject *switch_bar, GParamSpec *pspec, gpointer data){
	int toggle = kemoview_toggle_object_properties(SPHEREGRID_SWITCH);
	return;
};
static void coastline_radius_CB(GtkWidget *entry, gpointer data)
{
	double radius = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_coastline_radius(radius);
/*	printf("radius %d\n", radius);*/
}

static void set_shading_mode_CB(GtkComboBox *combobox_shading, gpointer user_data)
{
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_shading);
    GtkTreeIter iter;
    cairo_t *cr;
    
    gchar *row_string;
    int index_field;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_shading);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_MATH, &index_mode, -1);
    
    printf("Selected mode %d, %s\n", index_mode, row_string);
	kemoview_set_object_property_flags(SHADING_SWITCH, index_mode);
	
//	draw_mesh_w_menu();
	return;
};

static void set_surface_direction_CB(GtkComboBox *combobox_surfdir, gpointer user_data)
{
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_surfdir);
    GtkTreeIter iter;
    cairo_t *cr;
    
    gchar *row_string;
    int index_field;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_surfdir);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_MATH, &index_mode, -1);
    
    printf("Selected mode %d, %s\n", index_mode, row_string);
	kemoview_set_object_property_flags(POLYGON_SWITCH, index_mode);
	
//	draw_mesh_w_menu();
	return;
};


void add_axis_menu_box(struct kemoviewer_type *kemoviewer_data, GtkWidget *box_out){
	GtkWidget *vbox;
	
	GtkWidget *hbox_axis, *hbox_sph_grid, *hbox_coastline;
	GtkWidget *switch_axis, *switch_sph_grid, *switch_coastline;
	
	GtkWidget *hbox_coast_radius, *hbox_org_radius;
	GtkWidget *spin_coast_radius;
	GtkAdjustment *adj_coast_radius;
	double current_radius;
	char current_radius_text[30];
	
	GtkWidget *hbox_viewtype;
	GtkWidget *combobox_viewtype;
	GtkWidget *label_tree_viewtype;
	GtkCellRenderer *renderer_viewtype;
	GtkTreeModel *model_viewtype;
	GtkTreeModel *child_model_viewtype;
	
	GtkWidget *hbox_viewmatrix_save;
	GtkWidget *entry_viewmatrix_file;
	GtkWidget *saveView_Button, *loadView_Button;
	
	GtkWidget *hbox_shading;
	GtkWidget *combobox_shading;
	GtkWidget *label_tree_shading;
	GtkCellRenderer *renderer_shading;
	GtkTreeModel *model_shading;
	GtkTreeModel *child_model_shading;
	
	GtkWidget *hbox_surf_dir;
	GtkWidget *combobox_surf_dir;
	GtkWidget *label_tree_surf_dir;
	GtkCellRenderer *renderer_surf_dir;
	GtkTreeModel *model_surf_dir;
	GtkTreeModel *child_model_surf_dir;
	
	int index = 0;
	int iflag_mode;
	
		
	/* Set buttons   */
	label_tree_shading = create_fixed_label_w_index_tree();
	model_shading = gtk_tree_view_get_model (label_tree_shading);  
	child_model_shading = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_shading));
	index = 0;
	index = append_ci_item_to_tree(index, "Smooth surface", SMOOTH_SHADE, child_model_shading);
	index = append_ci_item_to_tree(index, "Flat surface", FLAT_SHADE, child_model_shading);
	
	combobox_shading = gtk_combo_box_new_with_model(child_model_shading);
	renderer_shading = gtk_cell_renderer_text_new();
	iflag_mode = kemoview_get_object_property_flags(SHADING_SWITCH);
	if(iflag_mode == FLAT_SHADE){
		gtk_combo_box_set_active(combobox_shading, 1);
	} else {
		gtk_combo_box_set_active(combobox_shading, 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_shading), renderer_shading, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_shading), renderer_shading,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_shading), "changed", 
				G_CALLBACK(set_shading_mode_CB), NULL);
	
	
	label_tree_surf_dir = create_fixed_label_w_index_tree();
	model_surf_dir = gtk_tree_view_get_model (label_tree_surf_dir);  
	child_model_surf_dir = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_surf_dir));
	index = 0;
	index = append_ci_item_to_tree(index, "Normal surface", NORMAL_POLYGON, child_model_surf_dir);
	index = append_ci_item_to_tree(index, "Reverse surface", REVERSE_POLYGON, child_model_surf_dir);
	
	combobox_surf_dir = gtk_combo_box_new_with_model(child_model_surf_dir);
	renderer_surf_dir = gtk_cell_renderer_text_new();
	iflag_mode = kemoview_get_object_property_flags(POLYGON_SWITCH);
	if(iflag_mode == REVERSE_POLYGON){
		gtk_combo_box_set_active(combobox_surf_dir, 1);
	} else {
		gtk_combo_box_set_active(combobox_surf_dir, 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_surf_dir), renderer_surf_dir, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_surf_dir), renderer_surf_dir,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_surf_dir), "changed", 
				G_CALLBACK(set_surface_direction_CB), NULL);
	
	
	switch_axis = gtk_switch_new();
	if(kemoview_get_object_property_flags(AXIS_TOGGLE) == 0){
		gtk_switch_set_active(GTK_SWITCH(switch_axis), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(switch_axis), TRUE);
	};
	g_signal_connect(G_OBJECT(switch_axis), "notify::active",
				G_CALLBACK(draw_axis_switch_CB), NULL);
	
	switch_coastline = gtk_switch_new();
	if(kemoview_get_object_property_flags(COASTLINE_SWITCH) == 0){
		gtk_switch_set_active(GTK_SWITCH(switch_coastline), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(switch_coastline), TRUE);
	};
	g_signal_connect(G_OBJECT(switch_coastline), "notify::active",
				G_CALLBACK(draw_coastline_switch_CB), NULL);
	
	switch_sph_grid = gtk_switch_new();
	if(kemoview_get_object_property_flags(SPHEREGRID_SWITCH) == 0){
		gtk_switch_set_active(GTK_SWITCH(switch_sph_grid), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(switch_sph_grid), TRUE);
	};
	g_signal_connect(G_OBJECT(switch_sph_grid), "notify::active",
				G_CALLBACK(draw_sph_grid_switch_CB), NULL);
	
	current_radius = kemoview_get_coastline_radius();
	sprintf(current_radius_text, "    %e    ", current_radius);
	adj_coast_radius = gtk_adjustment_new(current_radius, 0.0, 10.0, 0.02, 0.02, 0.0);
	spin_coast_radius = gtk_spin_button_new(GTK_ADJUSTMENT(adj_coast_radius), 0, 3);
	g_signal_connect(spin_coast_radius, "value-changed", G_CALLBACK(coastline_radius_CB),NULL);
	
	
	hbox_shading = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_shading), gtk_label_new("Shading mode: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_shading), combobox_shading, FALSE, FALSE, 0);
	
	hbox_surf_dir = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_surf_dir), gtk_label_new("Surface direction: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_surf_dir), combobox_surf_dir, FALSE, FALSE, 0);
	
	hbox_axis = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_axis), gtk_label_new("Draw axis: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_axis), switch_axis, FALSE, FALSE, 0);
	
	hbox_coastline = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_coastline), gtk_label_new("Draw coastline: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_coastline), switch_coastline, FALSE, FALSE, 0);
	
	hbox_sph_grid = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_sph_grid), gtk_label_new("Draw sphere grid: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_sph_grid), switch_sph_grid, FALSE, FALSE, 0);
	
	hbox_org_radius = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_org_radius), gtk_label_new("Current distance: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_org_radius), gtk_label_new(current_radius_text), TRUE, TRUE, 0);
	hbox_coast_radius = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_coast_radius), gtk_label_new("Radius: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_coast_radius), spin_coast_radius, TRUE, TRUE, 0);
	
	
	vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_axis, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_coastline, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_sph_grid, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_org_radius, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_coast_radius, TRUE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox), hbox_shading, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_surf_dir, TRUE, FALSE, 0);
	
	wrap_into_frame_gtk("Axis and grids", vbox, box_out);
	return;
}
