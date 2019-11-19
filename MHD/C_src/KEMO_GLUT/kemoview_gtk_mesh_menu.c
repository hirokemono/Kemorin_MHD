/*
 *  kemoview_gtk_mesh_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_mesh_menu.h"

static void subdoain_distance_CB(GtkWidget *entry, gpointer data)
{
	double dist = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	if(dist >= 0.0) kemoview_set_domain_distance(dist);
	kemoview_draw_with_modified_domain_distance();
	
	draw_full();
}

static void node_size_CB(GtkWidget *entry, gpointer data)
{
	double size = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	if(size >= 0.0) kemoview_set_node_diamater(size);
	
	draw_full();
}

static void num_color_loop_CB(GtkWidget *entry, gpointer data)
{
	int nloop = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	kemoview_set_num_of_color_loop(nloop);
	
	draw_full();
}

static void set_mesh_color_mode_CB(GtkComboBox *combobox_sfcolor, gpointer user_data)
{
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_sfcolor);
    GtkTreeIter iter;
    
    gchar *row_string;
    int index_field;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_sfcolor);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_MATH, &index_mode, -1);
    
    printf("Selected mode %d, %s\n", index_mode, row_string);
	kemoview_set_mesh_color_mode(index_mode);
	
	draw_full();
	return;
};


void add_gtk_mesh_menu(GtkWidget *window, struct kemoview_mesh_view *mesh_vws){
	
	GtkWidget *hbox_distance;
	GtkWidget *spin_dist;
	GtkAdjustment *adj_dist;
	double current_distance;
	
	GtkWidget *hbox_node_size;
	GtkWidget *spin_node_size;
	GtkAdjustment *adj_node_size;
	double current_size;
	
	GtkWidget *hbox_num_loop;
	GtkWidget *spin_num_loop;
	GtkAdjustment *adj_num_loop;
	int current_num_loop;
	
	GtkWidget *hbox_color_mode;
	GtkWidget *combobox_color_mode;
	GtkWidget *label_tree_color_mode;
	GtkCellRenderer *renderer_color_mode;
	GtkTreeModel *model_color_mode;
	GtkTreeModel *child_model_color_mode;
	
	int index = 0;
	int iflag_mode;
	
	/* Set buttons   */
	
	current_distance = kemoview_get_domain_distance();
	adj_dist = gtk_adjustment_new(current_distance, 0.0, 10.0, 0.005, 0.005, 0.0);
	spin_dist = gtk_spin_button_new(GTK_ADJUSTMENT(adj_dist), 0, 3);
	g_signal_connect(spin_dist, "value-changed", G_CALLBACK(subdoain_distance_CB),NULL);
	
	current_size = kemoview_get_node_diamater();
	adj_node_size = gtk_adjustment_new(current_size, 0.0, 10.0, 0.00001, 0.00001, 0.0);
	spin_node_size = gtk_spin_button_new(GTK_ADJUSTMENT(adj_node_size), 0, 3);
	g_signal_connect(spin_node_size, "value-changed", G_CALLBACK(node_size_CB),NULL);
	
	current_num_loop = kemoview_get_num_of_color_loop();
	adj_num_loop = gtk_adjustment_new(current_num_loop, 0, 100, 1, 1, 0.0);
	spin_num_loop = gtk_spin_button_new(GTK_ADJUSTMENT(adj_num_loop), 0, 0);
	g_signal_connect(spin_num_loop, "value-changed", G_CALLBACK(num_color_loop_CB),NULL);
	
	
	label_tree_color_mode = create_fixed_label_w_index_tree();
	model_color_mode = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_color_mode));  
	child_model_color_mode = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_color_mode));
	index = 0;
	index = append_ci_item_to_tree(index, "Rainbow",   RAINBOW_COLOR, child_model_color_mode);
	index = append_ci_item_to_tree(index, "Grayscale", GRAYSCALE, child_model_color_mode);
	
	
	combobox_color_mode = gtk_combo_box_new_with_model(child_model_color_mode);
	renderer_color_mode = gtk_cell_renderer_text_new();
	iflag_mode = kemoview_get_mesh_color_mode();
	if(iflag_mode == GRAYSCALE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_color_mode), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_color_mode), 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_color_mode), renderer_color_mode, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_color_mode), renderer_color_mode,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_color_mode), "changed", 
				G_CALLBACK(set_mesh_color_mode_CB), NULL);
	
	
	hbox_distance = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_distance), gtk_label_new("Subdomain distance: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_distance), spin_dist, TRUE, TRUE, 0);
	
	hbox_node_size = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_node_size), gtk_label_new("Node size: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_node_size), spin_node_size, TRUE, TRUE, 0);
	
	hbox_color_mode = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_color_mode), gtk_label_new("Color mode: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_color_mode), combobox_color_mode, FALSE, FALSE, 0);
	
	hbox_num_loop = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_num_loop), gtk_label_new("# of color loop: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_num_loop), spin_num_loop, TRUE, TRUE, 0);
	
	gtk_container_add(GTK_CONTAINER(window), mesh_vws->box_out);
	
	gtk_box_pack_start(GTK_BOX(mesh_vws->box_out), hbox_distance, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(mesh_vws->box_out), hbox_node_size, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(mesh_vws->box_out), hbox_color_mode, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mesh_vws->box_out), hbox_num_loop, TRUE, TRUE, 0);
	
	mesh_vws->domain_group_gmenu->box_grp = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	add_domain_draw_box(window, mesh_vws->domain_group_gmenu);
	wrap_into_expanded_frame_gtk("Domain", 400, 300, mesh_vws->domain_group_gmenu->box_grp, mesh_vws->box_out);
	
	mesh_vws->node_group_gmenu->box_grp = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	add_nod_group_draw_box(window, mesh_vws->node_group_gmenu);
	wrap_into_expanded_frame_gtk("Node group", 400, 300, mesh_vws->node_group_gmenu->box_grp, mesh_vws->box_out);

	mesh_vws->ele_group_gmenu->box_grp = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	add_ele_group_draw_box(window, mesh_vws->ele_group_gmenu);
	wrap_into_expanded_frame_gtk("Element group", 400, 300, mesh_vws->ele_group_gmenu->box_grp, mesh_vws->box_out);

	mesh_vws->surf_group_gmenu->box_grp = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	add_surf_group_draw_box(window, mesh_vws->surf_group_gmenu);
	wrap_into_expanded_frame_gtk("Surface group", 400, 300, mesh_vws->surf_group_gmenu->box_grp, mesh_vws->box_out);

	set_domain_draw_box(mesh_vws->domain_group_gmenu);
	set_nod_group_draw_box(mesh_vws->node_group_gmenu);
	set_ele_group_draw_box(mesh_vws->ele_group_gmenu);
	set_surf_group_draw_box(mesh_vws->surf_group_gmenu);
	return;
}
