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
	double current_size;
	int i_digit;
	double floatvalue = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));

	kemoview_get_node_diamater(&current_size, &i_digit);
	kemoview_set_node_diamater(floatvalue, i_digit);
	
	draw_full();
}

static void node_digit_CB(GtkWidget *entry, gpointer data)
{
	double current_size;
	int i_digit;
	int intvalue = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	
	kemoview_get_node_diamater(&current_size, &i_digit);
	kemoview_set_node_diamater(current_size, intvalue);
	
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

void set_gtk_mesh_menu(struct kemoview_mesh_view *mesh_vws){
	double current_size;
	int i_digit;

	int iflag_mode = kemoview_get_mesh_color_mode();
	int current_num_loop = kemoview_get_num_of_color_loop();
	double current_distance = kemoview_get_domain_distance();
	kemoview_get_node_diamater(&current_size, &i_digit);

	gtk_spin_button_set_value(GTK_SPIN_BUTTON(mesh_vws->spin_dist), current_distance);
	
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(mesh_vws->spin_node_size), current_size);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(mesh_vws->spin_node_digit), i_digit);
	
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(mesh_vws->spin_num_loop), current_num_loop);
	
	if(iflag_mode == GRAYSCALE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(mesh_vws->combobox_color_mode), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(mesh_vws->combobox_color_mode), 0);
	};
	return;
};

void add_gtk_mesh_menu(struct kemoview_mesh_view *mesh_vws,
                       GtkWidget *window, GtkWidget *vbox_add){	
	GtkWidget *hbox_distance;
	GtkAdjustment *adj_dist;
	
	GtkWidget *hbox_node_size;
	GtkAdjustment *adj_node_size, *adj_node_digit;
	
	GtkWidget *hbox_num_loop;
	GtkAdjustment *adj_num_loop;
	
	GtkWidget *hbox_color_mode;
	GtkWidget *label_tree_color_mode;
	GtkCellRenderer *renderer_color_mode;
	GtkTreeModel *model_color_mode;
	GtkTreeModel *child_model_color_mode;
	
	int index = 0;
	
	/* Set buttons   */
	
	adj_dist = gtk_adjustment_new(0.0, 0.0, 10.0, 0.005, 0.005, 0.0);
	mesh_vws->spin_dist = gtk_spin_button_new(GTK_ADJUSTMENT(adj_dist), 0, 3);
	g_signal_connect(mesh_vws->spin_dist, "value-changed", G_CALLBACK(subdoain_distance_CB),NULL);
	
	adj_node_size = gtk_adjustment_new(1, 1, 9, 1, 1, 0);
	mesh_vws->spin_node_size = gtk_spin_button_new(GTK_ADJUSTMENT(adj_node_size), 0, 3);
	g_signal_connect(mesh_vws->spin_node_size, "value-changed", G_CALLBACK(node_size_CB),NULL);
	
	adj_node_digit = gtk_adjustment_new(-3, -10, 10, 1, 1, 0);
	mesh_vws->spin_node_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_node_digit), 0, 0);
	g_signal_connect(mesh_vws->spin_node_digit, "value-changed", G_CALLBACK(node_digit_CB),NULL);
	
	adj_num_loop = gtk_adjustment_new(5, 0, 100, 1, 1, 0.0);
	mesh_vws->spin_num_loop = gtk_spin_button_new(GTK_ADJUSTMENT(adj_num_loop), 0, 0);
	g_signal_connect(mesh_vws->spin_num_loop, "value-changed", G_CALLBACK(num_color_loop_CB),NULL);
	
	label_tree_color_mode = create_fixed_label_w_index_tree();
	model_color_mode = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_color_mode));  
	child_model_color_mode = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_color_mode));
	index = 0;
	index = append_ci_item_to_tree(index, "Rainbow",   RAINBOW_COLOR, child_model_color_mode);
	index = append_ci_item_to_tree(index, "Grayscale", GRAYSCALE, child_model_color_mode);
	
	renderer_color_mode = gtk_cell_renderer_text_new();
	mesh_vws->combobox_color_mode = gtk_combo_box_new_with_model(child_model_color_mode);
	gtk_combo_box_set_active(GTK_COMBO_BOX(mesh_vws->combobox_color_mode), 0);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(mesh_vws->combobox_color_mode), renderer_color_mode, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(mesh_vws->combobox_color_mode), renderer_color_mode,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(mesh_vws->combobox_color_mode), "changed", 
				G_CALLBACK(set_mesh_color_mode_CB), NULL);
	
	set_gtk_mesh_menu(mesh_vws);
	
	hbox_distance = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_distance), gtk_label_new("Subdomain distance: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_distance), mesh_vws->spin_dist, TRUE, TRUE, 0);
	
	hbox_node_size = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_node_size), gtk_label_new("Node size: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_node_size), mesh_vws->spin_node_size, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_node_size), gtk_label_new("x 10^ "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_node_size), mesh_vws->spin_node_digit, TRUE, TRUE, 0);
	
	hbox_color_mode = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_color_mode), gtk_label_new("Color mode: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_color_mode), mesh_vws->combobox_color_mode, FALSE, FALSE, 0);
	
	hbox_num_loop = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_num_loop), gtk_label_new("Number of color loop: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_num_loop), mesh_vws->spin_num_loop, TRUE, TRUE, 0);
	
    GtkWidget *expander_domain = init_domain_draw_expander(window, mesh_vws->domain_group_gmenu);
	GtkWidget *expander_node = init_nod_group_draw_expander(window, mesh_vws->node_group_gmenu);
	GtkWidget *expander_ele = init_ele_group_draw_expander(window, mesh_vws->ele_group_gmenu);
	GtkWidget *expander_surf = init_surf_group_draw_expander(window, mesh_vws->surf_group_gmenu);
    
    gtk_box_pack_start(GTK_BOX(vbox_add), hbox_distance, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_add), hbox_node_size, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_add), hbox_color_mode, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_add), hbox_num_loop, FALSE, FALSE, 0);
    
    gtk_box_pack_start(GTK_BOX(vbox_add), expander_domain, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_add), expander_node, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_add), expander_ele, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_add), expander_surf, FALSE, FALSE, 0);
	return;
}

