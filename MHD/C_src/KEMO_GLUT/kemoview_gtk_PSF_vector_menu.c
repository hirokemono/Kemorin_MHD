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
	
	draw_full();
	return;
};

static void set_vector_mode_cb(GtkComboBox *combobox_cmap, gpointer user_data)
{
    int index_mode = gtk_selected_combobox_index(combobox_cmap);
    
	kemoview_set_PSF_tangential_vec_mode(index_mode);
	
	draw_full();
    return;
}

static void set_vector_color_cb(GtkComboBox *combobox_cmap, gpointer user_data)
{
    int index_mode = gtk_selected_combobox_index(combobox_cmap);
    
	kemoview_select_PSF_draw_switch(index_mode);
	
	draw_full();
    return;
}

static void set_ref_vector_CB(GtkWidget *entry, gpointer user_data)
{
	double gtk_floatvalue = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_PSF_vector_scale(gtk_floatvalue);
	
	draw_full();
	return;
}

static void set_vect_increment_CB(GtkWidget *entry, gpointer user_data)
{
	int gtk_intvalue = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	kemoview_set_PSF_color_param(ISET_VECTOR_INC, gtk_intvalue);
	
	draw_full();
	return;
}

static void set_vector_width_CB(GtkWidget *entry, gpointer user_data)
{
	struct colormap_view *color_vws = (struct colormap_view *) user_data;
	double gtk_floatvalue = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_PSF_vector_thickness(gtk_floatvalue);
	
	draw_full();
	return;
}


void make_gtk_psf_vector_menu(struct colormap_view *color_vws){
	GtkWidget *hbox_draw;
	GtkWidget *hbox_vecmode, *hbox_veccolor;
	GtkWidget *hbox_12, *hbox_13;
	GtkWidget *hbox_22, *hbox_23;
	GtkWidget *hbox_32, *hbox_33;
	GtkWidget *vbox_vec;
	
	GtkWidget *switch_1;
	
	GtkWidget *combobox_vecmode;
	GtkWidget *label_tree_vmode;
	GtkCellRenderer *renderer_vmode;
	GtkTreeModel *model_vmode;
	GtkTreeModel *child_model_vmode;
	
	GtkWidget *combobox_veccolor;
	GtkWidget *label_tree_veccolor;
	GtkCellRenderer *renderer_veccolor;
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
	model_vmode = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_vmode));  
	child_model_vmode = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_vmode));
	index = 0;
	index = append_ci_item_to_tree(index, "Full", FULL_COMPONENT, child_model_vmode);
	index = append_ci_item_to_tree(index, "Tangential", TANGENTIAL_COMPONENT, child_model_vmode);
	
	combobox_vecmode = gtk_combo_box_new_with_model(child_model_vmode);
	renderer_vmode = gtk_cell_renderer_text_new();
	iflag = kemoview_get_PSF_draw_flags(PSFTANVEC_TOGGLE);
	if(iflag == TANGENTIAL_COMPONENT){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_vecmode), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_vecmode), 0);
	}
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_vecmode), renderer_vmode, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_vecmode), renderer_vmode,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_vecmode), "changed", 
				G_CALLBACK(set_vector_mode_cb), NULL);
	
	label_tree_veccolor = create_fixed_label_w_index_tree();
	model_veccolor = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_veccolor));  
	child_model_veccolor = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_veccolor));
	index = 0;
	index = append_ci_item_to_tree(index, "Colored", RAINBOW_PSF_VECT, child_model_veccolor);
	index = append_ci_item_to_tree(index, "White",   WHITE_PSF_VECT, child_model_veccolor);
	
	combobox_veccolor = gtk_combo_box_new_with_model(child_model_veccolor);
	renderer_veccolor = gtk_cell_renderer_text_new();
	iflag = kemoview_get_PSF_color_param(ISET_VECTOR_COLOR);
	if(iflag == WHITE_PSF_VECT){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_veccolor), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_veccolor), 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_veccolor), renderer_veccolor, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_veccolor), renderer_veccolor,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_veccolor), "changed", 
				G_CALLBACK(set_vector_color_cb), NULL);
	
	
	current_ref_vector = kemoview_get_PSF_vector_scale();
	sprintf(current_ref_vect_txt, "    %e    ", current_ref_vector);
	adj_ref_vect = gtk_adjustment_new(current_ref_vector, 0.0, current_ref_vector*10.0, 0.01, 0.01, 0.0);
	spin_ref_vect = gtk_spin_button_new(GTK_ADJUSTMENT(adj_ref_vect), 0, 2);
	g_signal_connect(spin_ref_vect, "value-changed", G_CALLBACK(set_ref_vector_CB), (gpointer) color_vws);
	
	current_vec_increment = kemoview_get_PSF_color_param(ISET_VECTOR_INC);
	sprintf(current_vec_inc_txt, "    %d    ", current_vec_increment);
	adj_vect_inc = gtk_adjustment_new((double) current_vec_increment, 0, 500, 1, 1, 0.0);
	spin_vect_inc = gtk_spin_button_new(GTK_ADJUSTMENT(adj_vect_inc), 0, 0);
	g_signal_connect(spin_vect_inc, "value-changed", G_CALLBACK(set_vect_increment_CB), (gpointer) color_vws);
	
	current_vec_width = kemoview_get_PSF_vector_thickness();
	sprintf(current_vec_width_txt, "    %e    ", current_vec_width);
	adj_vect_width = gtk_adjustment_new(current_vec_width, 0.0, 1.0, 0.01, 0.01, 0.0);
	spin_vect_width = gtk_spin_button_new(GTK_ADJUSTMENT(adj_vect_width), 0, 4);
	g_signal_connect(spin_vect_width, "value-changed", G_CALLBACK(set_vector_width_CB), (gpointer) color_vws);
	
	
	hbox_draw = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_draw), gtk_label_new("Draw vector: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_draw), switch_1, FALSE, FALSE, 0);
	
	hbox_vecmode = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_vecmode), gtk_label_new("Direction: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_vecmode), combobox_vecmode, FALSE, FALSE, 0);
	
	hbox_veccolor = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_veccolor), gtk_label_new("Color: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_veccolor), combobox_veccolor, FALSE, FALSE, 0);
	
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
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_draw, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_vecmode, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_veccolor, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_13, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_12, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_23, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_22, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_33, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_32, TRUE, TRUE, 0);
	
	wrap_into_expanded_frame_gtk("Vector", 400, 300, vbox_vec, color_vws->psfVectorBox);
	return;
}
