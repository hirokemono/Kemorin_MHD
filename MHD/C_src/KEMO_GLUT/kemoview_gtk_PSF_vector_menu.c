/*
 *  kemoview_gtk_PSF_vector_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_PSF_vector_menu.h"

static void psf_vector_switch_CB(GObject *switch_vect, GParamSpec *pspec, gpointer user_data){
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) user_data;
    int iflag = gtk_switch_get_state(GTK_SWITCH(switch_vect));
    kemoview_set_PSF_draw_flags(PSFVECT_TOGGLE, iflag, kemo_sgl);

    draw_full(kemo_sgl);
	return;
};

static void set_vector_mode_cb(GtkComboBox *combobox_vecmode, gpointer user_data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) user_data;
    int index_mode = gtk_selected_combobox_index(combobox_vecmode);
    
	kemoview_set_PSF_tangential_vec_mode(index_mode, kemo_sgl);
    draw_full(kemo_sgl);
    return;
}

static void set_vector_color_cb(GtkComboBox *combobox_cmap, gpointer user_data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) user_data;
    int index_mode = gtk_selected_combobox_index(combobox_cmap);
    
	kemoview_set_PSF_color_param(ISET_VECTOR_COLOR, index_mode, kemo_sgl);
	
    draw_full(kemo_sgl);
    return;
}

static void set_ref_vector_CB(GtkWidget *entry, gpointer user_data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) user_data;
	double gtk_floatvalue = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	int i_digit;
	double current_value;
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_PSF_REFVECT,
                                      &current_value, &i_digit);
	kemoview_set_each_PSF_color_w_exp(ISET_PSF_REFVECT, gtk_floatvalue,
                                      i_digit, kemo_sgl);
	
    draw_full(kemo_sgl);
	return;
}

static void set_ref_digit_CB(GtkWidget *entry, gpointer user_data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) user_data;
	double gtk_intvalue = (double) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	int i_digit;
	double current_value;
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_PSF_REFVECT,
                                      &current_value, &i_digit);
	kemoview_set_each_PSF_color_w_exp(ISET_PSF_REFVECT, current_value,
                                      gtk_intvalue, kemo_sgl);
	
    draw_full(kemo_sgl);
	return;
}

static void set_vect_increment_CB(GtkWidget *entry, gpointer user_data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) user_data;
	double gtk_floatvalue = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	int i_digit;
	double current_value;
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_VECTOR_INC,
                                      &current_value, &i_digit);
	kemoview_set_each_PSF_color_w_exp(ISET_VECTOR_INC, gtk_floatvalue,
                                      i_digit, kemo_sgl);
	
    draw_full(kemo_sgl);
	return;
}

static void set_increment_digit_CB(GtkWidget *entry, gpointer user_data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) user_data;
	int gtk_intvalue = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	int i_digit;
	double current_value;
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_VECTOR_INC,
                                      &current_value, &i_digit);
	kemoview_set_each_PSF_color_w_exp(ISET_VECTOR_INC, current_value,
                                      gtk_intvalue, kemo_sgl);
	
    draw_full(kemo_sgl);
	return;
}

static void set_vector_width_CB(GtkWidget *entry, gpointer user_data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) user_data;
	double gtk_floatvalue = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	int i_digit;
	double current_value;
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_PSF_V_THICK,
                                      &current_value, &i_digit);
	kemoview_set_each_PSF_color_w_exp(ISET_PSF_V_THICK, gtk_floatvalue,
                                      i_digit, kemo_sgl);
	
    draw_full(kemo_sgl);
	return;
}
static void set_width_digit_CB(GtkWidget *entry, gpointer user_data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) user_data;
	int gtk_intvalue = (double) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	int i_digit;
	double current_value;
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_PSF_V_THICK,
                                      &current_value, &i_digit);
	kemoview_set_each_PSF_color_w_exp(ISET_PSF_V_THICK, current_value,
                                      gtk_intvalue, kemo_sgl);
	
    draw_full(kemo_sgl);
	return;
}

void set_gtk_psf_vector_menu(struct kemoviewer_type *kemo_sgl,
                             struct psf_vector_gtk_menu *psf_vector_menu){
	int iflag;
	int i_digit;
	double current_value;
	
	if(kemoview_get_PSF_draw_flags(kemo_sgl, PSFVECT_TOGGLE) == 0){
		gtk_switch_set_active(GTK_SWITCH(psf_vector_menu->switch_vect), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(psf_vector_menu->switch_vect), TRUE);
	};
	
	iflag = kemoview_get_PSF_draw_flags(kemo_sgl, PSFTANVEC_TOGGLE);
	if(iflag == TANGENTIAL_COMPONENT){
		gtk_combo_box_set_active(GTK_COMBO_BOX(psf_vector_menu->combobox_vecmode), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(psf_vector_menu->combobox_vecmode), 0);
	}
	
	iflag = kemoview_get_PSF_color_param(kemo_sgl, ISET_VECTOR_COLOR);
	if(iflag == WHITE_SURFACE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(psf_vector_menu->combobox_veccolor), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(psf_vector_menu->combobox_veccolor), 0);
	};
	
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_PSF_REFVECT, 
                                      &current_value, &i_digit);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(psf_vector_menu->spin_ref_vect), current_value);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(psf_vector_menu->spin_ref_digit), i_digit);
	
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_VECTOR_INC, 
                                      &current_value, &i_digit);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(psf_vector_menu->spin_vect_inc), current_value);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(psf_vector_menu->spin_inc_digit), i_digit);
	
	kemoview_get_each_PSF_color_w_exp(kemo_sgl, ISET_PSF_V_THICK,
                                      &current_value, &i_digit);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(psf_vector_menu->spin_vect_width), current_value);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(psf_vector_menu->spin_width_digit), i_digit);
	return;
};

GtkWidget * make_gtk_psf_vector_menu(struct kemoviewer_type *kemo_sgl, GtkWidget *window,
                                     struct psf_vector_gtk_menu *psf_vector_menu){
    GtkWidget *expander;
    
	GtkWidget *hbox_draw;
	GtkWidget *hbox_vecmode, *hbox_veccolor;
	GtkWidget *hbox_12, *hbox_22, *hbox_32;
	GtkWidget *vbox_vec;
	
	GtkWidget *label_tree_vmode;
	GtkCellRenderer *renderer_vmode;
	GtkTreeModel *model_vmode;
	GtkTreeModel *child_model_vmode;
	
	GtkWidget *label_tree_veccolor;
	GtkCellRenderer *renderer_veccolor;
	GtkTreeModel *model_veccolor;
	GtkTreeModel *child_model_veccolor;
	
	int index = 0;
    int iflag;
	
	GtkAdjustment *adj_ref_vect, *adj_ref_digit;
	GtkAdjustment *adj_vect_inc, *adj_inc_digit;
	GtkAdjustment *adj_vect_width, *adj_width_digit;
	
	
	psf_vector_menu->switch_vect = gtk_switch_new();
    iflag = kemoview_get_PSF_draw_flags(kemo_sgl, PSFVECT_TOGGLE);
    gtk_switch_set_state(GTK_SWITCH(psf_vector_menu->switch_vect), iflag);
	g_signal_connect(G_OBJECT(psf_vector_menu->switch_vect), "notify::active",
				G_CALLBACK(psf_vector_switch_CB), (gpointer) kemo_sgl);
	
	label_tree_vmode = create_fixed_label_w_index_tree();
	model_vmode = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_vmode));  
	child_model_vmode = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_vmode));
	index = 0;
	index = append_ci_item_to_tree(index, "Full", FULL_COMPONENT, child_model_vmode);
	index = append_ci_item_to_tree(index, "Tangential", TANGENTIAL_COMPONENT, child_model_vmode);
	
	renderer_vmode = gtk_cell_renderer_text_new();
	psf_vector_menu->combobox_vecmode = gtk_combo_box_new_with_model(child_model_vmode);
	gtk_combo_box_set_active(GTK_COMBO_BOX(psf_vector_menu->combobox_vecmode), 0);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(psf_vector_menu->combobox_vecmode), renderer_vmode, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(psf_vector_menu->combobox_vecmode), 
								   renderer_vmode, "text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(psf_vector_menu->combobox_vecmode), "changed", 
				G_CALLBACK(set_vector_mode_cb), (gpointer) kemo_sgl);
	
	label_tree_veccolor = create_fixed_label_w_index_tree();
	model_veccolor = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_veccolor));  
	child_model_veccolor = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_veccolor));
	index = 0;
	index = append_ci_item_to_tree(index, "Colored", RAINBOW_SURFACE, child_model_veccolor);
	index = append_ci_item_to_tree(index, "White",   WHITE_SURFACE, child_model_veccolor);
	
	renderer_veccolor = gtk_cell_renderer_text_new();
	psf_vector_menu->combobox_veccolor = gtk_combo_box_new_with_model(child_model_veccolor);
	gtk_combo_box_set_active(GTK_COMBO_BOX(psf_vector_menu->combobox_veccolor), 0);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(psf_vector_menu->combobox_veccolor),
							   renderer_veccolor, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(psf_vector_menu->combobox_veccolor),
								   renderer_veccolor, "text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(psf_vector_menu->combobox_veccolor), "changed", 
				G_CALLBACK(set_vector_color_cb), (gpointer) kemo_sgl);
	
	
	adj_ref_vect = gtk_adjustment_new(1, 1, 9, 1, 1, 0);
	psf_vector_menu->spin_ref_vect = gtk_spin_button_new(GTK_ADJUSTMENT(adj_ref_vect), 0, 0);
	g_signal_connect(psf_vector_menu->spin_ref_vect, "value-changed", 
                     G_CALLBACK(set_ref_vector_CB), (gpointer) kemo_sgl);
	
	adj_ref_digit = gtk_adjustment_new(0, -10, 10, 1, 1, 0);
	psf_vector_menu->spin_ref_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_ref_digit), 0, 0);
	g_signal_connect(psf_vector_menu->spin_ref_digit, "value-changed",
                     G_CALLBACK(set_ref_digit_CB), (gpointer) kemo_sgl);
	
	adj_vect_inc = gtk_adjustment_new(1, 1, 9, 1, 1, 0);
	psf_vector_menu->spin_vect_inc = gtk_spin_button_new(GTK_ADJUSTMENT(adj_vect_inc), 0, 0);
	g_signal_connect(psf_vector_menu->spin_vect_inc, "value-changed",
					 G_CALLBACK(set_vect_increment_CB), (gpointer) kemo_sgl);
	
	adj_inc_digit = gtk_adjustment_new(0, 0, 10, 1, 1, 0);
	psf_vector_menu->spin_inc_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_inc_digit), 0, 0);
	g_signal_connect(psf_vector_menu->spin_inc_digit, "value-changed",
					 G_CALLBACK(set_increment_digit_CB), (gpointer) kemo_sgl);
	
	adj_vect_width = gtk_adjustment_new(1, 0, 9, 1, 1, 0);
	psf_vector_menu->spin_vect_width = gtk_spin_button_new(GTK_ADJUSTMENT(adj_vect_width), 0, 0);
	g_signal_connect(psf_vector_menu->spin_vect_width, "value-changed",
					 G_CALLBACK(set_vector_width_CB), (gpointer) kemo_sgl);
	
	adj_width_digit = gtk_adjustment_new(-3, -10, 10, 1, 1, 0);
	psf_vector_menu->spin_width_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_width_digit), 0, 0);
	g_signal_connect(psf_vector_menu->spin_width_digit, "value-changed",
					 G_CALLBACK(set_width_digit_CB), (gpointer) kemo_sgl);
	
	hbox_draw = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_draw), gtk_label_new("Draw vector: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_draw), psf_vector_menu->switch_vect, FALSE, FALSE, 0);
	
	hbox_vecmode = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_vecmode), gtk_label_new("Direction: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_vecmode), psf_vector_menu->combobox_vecmode, FALSE, FALSE, 0);
	
	hbox_veccolor = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_veccolor), gtk_label_new("Color: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_veccolor), psf_vector_menu->combobox_veccolor, FALSE, FALSE, 0);
	
	hbox_12 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_12), gtk_label_new("Reference: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_12), psf_vector_menu->spin_ref_vect, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_12), gtk_label_new("X 10^"), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_12), psf_vector_menu->spin_ref_digit, TRUE, TRUE, 0);
	
	hbox_32 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_32), gtk_label_new("Arrow width: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_32), psf_vector_menu->spin_vect_width, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_32), gtk_label_new("X 10^"), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_32), psf_vector_menu->spin_width_digit, TRUE, TRUE, 0);
	
	hbox_22 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_22), gtk_label_new("Increment: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_22), psf_vector_menu->spin_vect_inc, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_22), gtk_label_new("X 10^"), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_22), psf_vector_menu->spin_inc_digit, TRUE, TRUE, 0);
	
	vbox_vec = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_draw, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_vecmode, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_veccolor, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_12, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_32, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_vec), hbox_22, TRUE, TRUE, 0);
	
	expander = wrap_into_scroll_expansion_gtk("Vector", 425, 300, window, vbox_vec);
	return expander;
}
