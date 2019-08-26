/*
 *  kemoview_gtk_PSF_vector_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_PSF_vector_menu.h"

static void psf_vector_switch_CB(GObject *switch_1, GParamSpec *pspec, gpointer data){
	kemoview_select_PSF_draw_switch(PSFVECT_TOGGLE);
	return;
};

static void set_vector_mode_cb(GtkComboBox *combobox_cmap, gpointer user_data)
{
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_cmap);
    GtkTreeIter iter;
    cairo_t *cr;
    
    gchar *row_string;
    int index_field;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_cmap);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_MATH, &index_mode, -1);
    
    printf("Selected mode %d, %s\n", index_mode, row_string);
	kemoview_set_PSF_tangential_vec_mode(index_mode);
    return;
}

static void set_vector_color_cb(GtkComboBox *combobox_cmap, gpointer user_data)
{
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_cmap);
    GtkTreeIter iter;
    cairo_t *cr;
    
    gchar *row_string;
    int index_field;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_cmap);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_MATH, &index_mode, -1);
    
    printf("Selected mode %d, %s\n", index_mode, row_string);
	kemoview_select_PSF_draw_switch(index_mode);
    return;
}

static void set_ref_vector_CB(GtkWidget *entry, gpointer user_data)
{
	struct colormap_view *color_vws = (struct colormap_view *) user_data;
	double gtk_floatvalue = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_PSF_vector_scale(gtk_floatvalue);
	return;
}

static void set_vect_increment_CB(GtkWidget *entry, gpointer user_data)
{
	struct colormap_view *color_vws = (struct colormap_view *) user_data;
	int gtk_intvalue = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	kemoview_set_PSF_vector_increment(gtk_intvalue);
	return;
}

static void set_vector_width_CB(GtkWidget *entry, gpointer user_data)
{
	struct colormap_view *color_vws = (struct colormap_view *) user_data;
	double gtk_floatvalue = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_PSF_vector_thickness(gtk_floatvalue);
	return;
}


void add_gtk_psf_vector_menu(struct colormap_view *color_vws, GtkWidget *box){
	GtkWidget *hbox_1;
	GtkWidget *hbox_12, *hbox_13;
	GtkWidget *hbox_22, *hbox_23;
	GtkWidget *hbox_32, *hbox_33;
	GtkWidget *expander_vec,  *scroll_vec, *Frame_vec;
	GtkWidget *hbox_vec,  *vbox_vec;
	
	GtkWidget *switch_1;
	int iflag_vect;
	
	GtkWidget *combobox_vecmode;
	GtkWidget *label_tree_vmode;
	GtkTreeModel *model_vmode;
	GtkTreeModel *child_model_vmode;
	
	GtkWidget *combobox_veccolor;
	GtkWidget *label_tree_veccolor;
	GtkTreeModel *model_veccolor;
	GtkTreeModel *child_model_veccolor;
	
	int index = 0;
	int iflag;
	
	GtkWidget *spin_ref_vect;
	GtkAdjustment *adj_ref_vect;
	double current_ref_vector;
	char current_ref_vect_txt[30];
	
	GtkWidget *spin_vect_inc;
	GtkAdjustment *adj_vect_inc;
	int current_vec_increment;
	char current_vec_inc_txt[30];
	
	GtkWidget *spin_vect_width;
	GtkAdjustment *adj_vect_width;
	double current_vec_width;
	char current_vec_width_txt[30];
	
	switch_1 = gtk_switch_new();
	if(kemoview_get_PSF_draw_flags(PSFVECT_TOGGLE) == 0){
		gtk_switch_set_active(GTK_SWITCH(switch_1), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(switch_1), TRUE);
	};
	g_signal_connect(G_OBJECT(switch_1), "notify::active",
				G_CALLBACK(psf_vector_switch_CB), NULL);
	
	label_tree_vmode = create_fixed_label_w_index_tree();
	model_vmode = gtk_tree_view_get_model (label_tree_vmode);  
	child_model_vmode = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_vmode));
	index = 0;
	index = append_ci_item_to_tree(index, "Full", FULL_COMPONENT, child_model_vmode);
	index = append_ci_item_to_tree(index, "Tangential", TANGENTIAL_COMPONENT, child_model_vmode);
	
	combobox_vecmode = gtk_combo_box_new_with_model(child_model_vmode);
	child_model_vmode = gtk_cell_renderer_text_new();
	iflag = kemoview_get_PSF_draw_flags(PSFTANVEC_TOGGLE);
	gtk_combo_box_set_active(combobox_vecmode, iflag);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_vecmode), child_model_vmode, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_vecmode), child_model_vmode,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_vecmode), "changed", 
				G_CALLBACK(set_vector_mode_cb), NULL);
	
	combobox_veccolor = create_fixed_label_w_index_tree();
	model_veccolor = gtk_tree_view_get_model (combobox_veccolor);  
	child_model_veccolor = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_veccolor));
	index = 0;
	index = append_ci_item_to_tree(index, "Colored", RAINBOW_PSF_VECT, child_model_veccolor);
	index = append_ci_item_to_tree(index, "White",   WHITE_PSF_VECT, child_model_veccolor);
	
	combobox_veccolor = gtk_combo_box_new_with_model(child_model_veccolor);
	child_model_veccolor = gtk_cell_renderer_text_new();
	iflag = kemoview_get_PSF_vector_color_mode();
	gtk_combo_box_set_active(combobox_veccolor, iflag);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_veccolor), child_model_veccolor, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_veccolor), child_model_veccolor,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_veccolor), "changed", 
				G_CALLBACK(set_vector_color_cb), NULL);
	
	
	current_ref_vector = kemoview_get_PSF_vector_scale();
	sprintf(current_ref_vect_txt, "    %e    ", current_ref_vector);
	adj_ref_vect = gtk_adjustment_new(current_ref_vector, 0.0, current_ref_vector*10.0, 0.01, 0.01, 0.0);
	spin_ref_vect = gtk_spin_button_new(GTK_ADJUSTMENT(adj_ref_vect), 0, 2);
	g_signal_connect(spin_ref_vect, "value-changed", G_CALLBACK(set_ref_vector_CB), color_vws);
	
	current_vec_increment = kemoview_get_PSF_vector_increment();
	sprintf(current_vec_inc_txt, "    %d    ", current_vec_increment);
	adj_vect_inc = gtk_adjustment_new((double) current_vec_increment, 0, 500, 1, 1, 0.0);
	spin_vect_inc = gtk_spin_button_new(GTK_ADJUSTMENT(adj_vect_inc), 0, 2);
	g_signal_connect(spin_vect_inc, "value-changed", G_CALLBACK(set_vect_increment_CB), color_vws);
	
	current_vec_width = kemoview_get_PSF_vector_thickness();
	sprintf(current_vec_width_txt, "    %e    ", current_vec_width);
	adj_vect_width = gtk_adjustment_new(current_vec_width, 0.0, 1.0, 0.01, 0.01, 0.0);
	spin_vect_width = gtk_spin_button_new(GTK_ADJUSTMENT(adj_vect_width), 0, 2);
	g_signal_connect(spin_vect_width, "value-changed", G_CALLBACK(set_vector_width_CB), color_vws);
	
	hbox_1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_1), gtk_label_new("Draw vector: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1), switch_1, FALSE, FALSE, 0);
	
	hbox_13 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_13), gtk_label_new("Current Vector ref.: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_13), gtk_label_new(current_ref_vect_txt), TRUE, TRUE, 0);
	hbox_12 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_12), gtk_label_new("Reference: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_12), spin_ref_vect, TRUE, TRUE, 0);
	
	hbox_23 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_23), gtk_label_new("Current Increment: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_23), gtk_label_new(current_vec_inc_txt), TRUE, TRUE, 0);
	hbox_22 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_22), gtk_label_new("Increment: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_22), spin_vect_inc, TRUE, TRUE, 0);
	
	hbox_33 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_33), gtk_label_new("Current width.: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_33), gtk_label_new(current_vec_width_txt), TRUE, TRUE, 0);
	hbox_32 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_32), gtk_label_new("Arrow width: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_32), spin_vect_width, TRUE, TRUE, 0);
	
	vbox_vec = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_vec), combobox_vecmode, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_vec), combobox_veccolor, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_13, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_12, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_23, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_22, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_33, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_32, TRUE, TRUE, 0);
	
	Frame_vec = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_vec), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame_vec), vbox_vec);
	
	hbox_vec = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_vec), gtk_label_new("  "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_vec), Frame_vec, TRUE, TRUE, 0);
	
	scroll_vec = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll_vec),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scroll_vec, 400, 300);
	gtk_container_add(GTK_CONTAINER(scroll_vec), hbox_vec);
	
	expander_vec = gtk_expander_new_with_mnemonic("Vector");
	gtk_container_add(GTK_CONTAINER(expander_vec), scroll_vec);
	
	gtk_box_pack_start(GTK_BOX(box), expander_vec, TRUE, FALSE, 0);
	return;
}
