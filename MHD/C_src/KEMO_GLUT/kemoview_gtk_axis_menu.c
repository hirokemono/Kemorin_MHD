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
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	kemoview_toggle_object_properties(AXIS_TOGGLE, kemo_sgl);
	
	draw_full(kemo_sgl);
	return;
};
static void draw_coastline_switch_CB(GObject *switch_bar, GParamSpec *pspec, gpointer data){
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	kemoview_toggle_object_properties(COASTLINE_SWITCH, kemo_sgl);
	
    draw_full(kemo_sgl);
	return;
};
static void draw_sph_grid_switch_CB(GObject *switch_bar, GParamSpec *pspec, gpointer data){
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	kemoview_toggle_object_properties(SPHEREGRID_SWITCH, kemo_sgl);
	
    draw_full(kemo_sgl);
	return;
};
static void coastline_radius_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	double radius = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_coastline_radius(radius, kemo_sgl);
	
    draw_full(kemo_sgl);
}

static void set_shading_mode_CB(GtkComboBox *combobox_shading, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    int index_mode = gtk_selected_combobox_index(combobox_shading);
    
	kemoview_set_object_property_flags(SHADING_SWITCH, index_mode, kemo_sgl);
    draw_full(kemo_sgl);
	return;
};

static void set_surface_direction_CB(GtkComboBox *combobox_surfdir, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    int index_mode = gtk_selected_combobox_index(combobox_surfdir);
	
	kemoview_set_object_property_flags(POLYGON_SWITCH, index_mode, kemo_sgl);
    draw_full(kemo_sgl);
	return;
};


GtkWidget * make_axis_menu_box(struct kemoviewer_type *kemo_sgl,
                               GtkWidget *window){
	int index = 0;
	
	/* Set buttons   */
	GtkWidget * label_tree_shading = create_fixed_label_w_index_tree();
	GtkTreeModel * model_shading = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_shading));  
	GtkTreeModel * child_model_shading = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_shading));
	index = 0;
	index = append_ci_item_to_tree(index, "Smooth surface", SMOOTH_SHADE, child_model_shading);
	index = append_ci_item_to_tree(index, "Flat surface", FLAT_SHADE, child_model_shading);
	
	GtkWidget *combobox_shading = gtk_combo_box_new_with_model(child_model_shading);
	GtkCellRenderer *renderer_shading = gtk_cell_renderer_text_new();
	if(kemoview_get_object_property_flags(kemo_sgl, SHADING_SWITCH) == FLAT_SHADE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_shading), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_shading), 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_shading), renderer_shading, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_shading), renderer_shading,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_shading), "changed", 
				G_CALLBACK(set_shading_mode_CB), (gpointer) kemo_sgl);
	
	
	GtkWidget *label_tree_surf_dir = create_fixed_label_w_index_tree();
	GtkTreeModel *model_surf_dir = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_surf_dir));  
	GtkTreeModel *child_model_surf_dir = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_surf_dir));
	index = 0;
	index = append_ci_item_to_tree(index, "Normal surface", NORMAL_POLYGON, child_model_surf_dir);
	index = append_ci_item_to_tree(index, "Reverse surface", REVERSE_POLYGON, child_model_surf_dir);
	
	GtkWidget *combobox_surf_dir = gtk_combo_box_new_with_model(child_model_surf_dir);
	GtkCellRenderer *renderer_surf_dir = gtk_cell_renderer_text_new();
	if(kemoview_get_object_property_flags(kemo_sgl, POLYGON_SWITCH) == REVERSE_POLYGON){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_surf_dir), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_surf_dir), 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_surf_dir), renderer_surf_dir, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_surf_dir), renderer_surf_dir,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_surf_dir), "changed", 
				G_CALLBACK(set_surface_direction_CB), (gpointer) kemo_sgl);
	
	
	GtkWidget *switch_axis = gtk_switch_new();
	if(kemoview_get_object_property_flags(kemo_sgl, AXIS_TOGGLE) == 0){
		gtk_switch_set_active(GTK_SWITCH(switch_axis), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(switch_axis), TRUE);
	};
	g_signal_connect(G_OBJECT(switch_axis), "notify::active",
				G_CALLBACK(draw_axis_switch_CB), (gpointer) kemo_sgl);
	
	GtkWidget *switch_coastline = gtk_switch_new();
	if(kemoview_get_object_property_flags(kemo_sgl, COASTLINE_SWITCH) == 0){
		gtk_switch_set_active(GTK_SWITCH(switch_coastline), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(switch_coastline), TRUE);
	};
	g_signal_connect(G_OBJECT(switch_coastline), "notify::active",
				G_CALLBACK(draw_coastline_switch_CB), (gpointer) kemo_sgl);
	
	GtkWidget *switch_sph_grid = gtk_switch_new();
	if(kemoview_get_object_property_flags(kemo_sgl, SPHEREGRID_SWITCH) == 0){
		gtk_switch_set_active(GTK_SWITCH(switch_sph_grid), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(switch_sph_grid), TRUE);
	};
	g_signal_connect(G_OBJECT(switch_sph_grid), "notify::active",
				G_CALLBACK(draw_sph_grid_switch_CB), (gpointer) kemo_sgl);
	
	GtkAdjustment *adj_coast_radius = gtk_adjustment_new(kemoview_get_coastline_radius(kemo_sgl),
										  0.0, 10.0, 0.02, 0.02, 0.0);
	GtkWidget *spin_coast_radius = gtk_spin_button_new(GTK_ADJUSTMENT(adj_coast_radius), 0, 3);
	g_signal_connect(spin_coast_radius, "value-changed",
                     G_CALLBACK(coastline_radius_CB), (gpointer) kemo_sgl);
	
	
	GtkWidget *hbox_shading = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_shading), gtk_label_new("Shading mode: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_shading), combobox_shading, FALSE, FALSE, 0);
	
	GtkWidget *hbox_surf_dir = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_surf_dir), gtk_label_new("Surface direction: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_surf_dir), combobox_surf_dir, FALSE, FALSE, 0);
	
	GtkWidget *hbox_axis = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_axis), gtk_label_new("Draw axis: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_axis), switch_axis, FALSE, FALSE, 0);
	
	GtkWidget *hbox_coastline = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_coastline), gtk_label_new("Draw coastline: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_coastline), switch_coastline, FALSE, FALSE, 0);
	
	GtkWidget *hbox_sph_grid = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_sph_grid), gtk_label_new("Draw sphere grid: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_sph_grid), switch_sph_grid, FALSE, FALSE, 0);
	
	GtkWidget *hbox_coast_radius = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_coast_radius), gtk_label_new("Radius: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_coast_radius), spin_coast_radius, FALSE, FALSE, 0);
	
	
	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_axis, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_coastline, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_sph_grid, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_coast_radius, TRUE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox), hbox_shading, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_surf_dir, TRUE, FALSE, 0);

    return wrap_into_scroll_expansion_gtk("Axis and grids", 320, 240, window, vbox);
}
