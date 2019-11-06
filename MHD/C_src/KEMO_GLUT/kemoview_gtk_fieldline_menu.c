/*
 *  kemoview_gtk_fieldline_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_fieldline_menu.h"

static void fline_thickness_CB(GtkWidget *entry, gpointer data)
{
	double current_thick;
	int current_digit;
	double thick_in = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	if(thick_in <= 0) return;
	
	kemoview_get_fline_thickness(&current_thick, &current_digit);
	kemoview_set_fline_thickness(thick_in, current_digit);

	draw_full();
}
static void fline_digit_CB(GtkWidget *entry, gpointer data)
{
	double current_thick;
	int current_digit;
	int in_digit = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	kemoview_get_fline_thickness(&current_thick, &current_digit);
	kemoview_set_fline_thickness(current_thick, in_digit);
	
	draw_full();
}

static void MinValueChange_CB(GtkWidget *entry, gpointer data)
{
	double gtk_floatvalue = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	int i_min_digit, i_max_digit;
	double minvalue, maxvalue;
	kemoview_get_fline_range_min(&minvalue, &i_min_digit);
	kemoview_get_fline_range_max(&maxvalue, &i_max_digit);
	kemoview_set_fline_linear_colormap(gtk_floatvalue, i_min_digit, 
									   maxvalue, i_max_digit);
	
	draw_full();
}

static void MinDigitChange_CB(GtkWidget *entry, gpointer data)
{
	int gtk_intvalue = (double) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	int i_min_digit, i_max_digit;
	double minvalue, maxvalue;
	kemoview_get_fline_range_min(&minvalue, &i_min_digit);
	kemoview_get_fline_range_max(&maxvalue, &i_max_digit);
	kemoview_set_fline_linear_colormap(minvalue, gtk_intvalue, 
									   maxvalue, i_max_digit);
	
	draw_full();
}

static void MaxValueChange_CB(GtkWidget *entry, gpointer data)
{
	double gtk_floatvalue = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	int i_min_digit, i_max_digit;
	double minvalue, maxvalue;
	kemoview_get_fline_range_min(&minvalue, &i_min_digit);
	kemoview_get_fline_range_max(&maxvalue, &i_max_digit);
	kemoview_set_fline_linear_colormap(minvalue, i_min_digit, 
									   gtk_floatvalue, i_max_digit);
	
	draw_full();
}

static void MaxDigitChange_CB(GtkWidget *entry, gpointer data)
{
	int gtk_intvalue = (double) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	int i_min_digit, i_max_digit;
	double minvalue, maxvalue;
	kemoview_get_fline_range_min(&minvalue, &i_min_digit);
	kemoview_get_fline_range_max(&maxvalue, &i_max_digit);
	kemoview_set_fline_linear_colormap(minvalue, i_min_digit, 
									   maxvalue, gtk_intvalue);
	
	draw_full();
}

static void psf_fieldtube_switch_CB(GObject *switch_1, GParamSpec *pspec, gpointer data){
	kemoview_toggle_fline_type();
	
	draw_full();
	return;
};

static void psf_fline_colormode_CB(GtkComboBox *combobox_sfcolor, gpointer user_data)
{
    int index_mode = gtk_selected_combobox_index(combobox_sfcolor);
	
	kemoview_set_fline_color_param(ISET_COLORMAP, index_mode);
	draw_full();
	return;
};


void add_gtk_fieldline_menu(GtkWidget *box_out){
	GtkWidget *hbox_tube, *hbox_color;
	GtkWidget *hbox_thickness;
	GtkWidget *hbox_range_min, *hbox_range_max;
	
	GtkWidget *combobox_color;
	GtkWidget *label_tree_color;
	GtkCellRenderer *renderer_color;
	GtkTreeModel *model_color;
	GtkTreeModel *child_model_color;
	
	int index = 0;
	
	GtkWidget *switch_tube;
	
	GtkWidget *spin_thick;
	GtkAdjustment *adj_thick;
	double current_thick;
	int int_thick;
	
	GtkWidget *spin_digit;
	GtkAdjustment *adj_digit;
	int current_digit;
	
	GtkWidget *spin_range_min, *spin_min_digit;
	GtkWidget *spin_range_max, *spin_max_digit;
	GtkAdjustment *adj_min_value, *adj_min_digit;
	GtkAdjustment *adj_max_value, *adj_max_digit;
	int num_fld, num_comp, ifield, icomp, itype_fline, icolor_mode;
	int i_min_digit, i_max_digit;
	double range_min, range_max;
	double value_min, value_max, delta;
	char min_text[40], max_text[40];
	
	num_fld = kemoview_get_fline_field_param(NUM_FIELD_FLAG);
	ifield =  kemoview_get_fline_field_param(FIELD_SEL_FLAG);
	icomp =   kemoview_get_fline_field_param(DRAW_ADDRESS_FLAG);
	num_comp = kemoview_get_fline_color_num_comps(ifield);
	itype_fline = kemoview_get_fline_field_param(LINETYPE_FLAG);
	icolor_mode = kemoview_get_fline_color_param(ISET_COLORMAP);
	
    value_min = kemoview_get_fline_data_range(ISET_COLOR_MIN, icomp);
	value_max = kemoview_get_fline_data_range(ISET_COLOR_MAX, icomp);

	label_tree_color = create_fixed_label_w_index_tree();
	model_color = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_color));  
	child_model_color = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_color));
	index = 0;
	index = append_ci_item_to_tree(index, "Rainbow lines", RAINBOW_LINE, child_model_color);
	index = append_ci_item_to_tree(index, "Two colord linees", TWO_COLOR_LINE, child_model_color);
	index = append_ci_item_to_tree(index, "Two grayscale linees", TWO_GRAY_LINE, child_model_color);
	index = append_ci_item_to_tree(index, "Black lines", BLACK_LINE, child_model_color);
	
	combobox_color = gtk_combo_box_new_with_model(child_model_color);
	renderer_color = gtk_cell_renderer_text_new();
	if(icolor_mode == BLACK_LINE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_color), 3);
	} else 	if(icolor_mode == TWO_GRAY_LINE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_color), 2);
	} else 	if(icolor_mode == TWO_COLOR_LINE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_color), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_color), 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_color), renderer_color, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_color), renderer_color,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_color), "changed", 
				G_CALLBACK(psf_fline_colormode_CB), NULL);
	
	
	switch_tube = gtk_switch_new();
	if(itype_fline == 0){
		gtk_switch_set_active(GTK_SWITCH(switch_tube), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(switch_tube), TRUE);
	};
	g_signal_connect(G_OBJECT(switch_tube), "notify::active",
				G_CALLBACK(psf_fieldtube_switch_CB), NULL);
	
	
	kemoview_get_fline_thickness(&current_thick, &current_digit);
	int_thick = (int) current_thick;

	adj_thick = gtk_adjustment_new(int_thick, 0.0, 9.0, 1, 1, 0.0);
	spin_thick = gtk_spin_button_new(GTK_ADJUSTMENT(adj_thick), 0, 0);
	g_signal_connect(spin_thick, "value-changed", G_CALLBACK(fline_thickness_CB),NULL);
	
	adj_digit = gtk_adjustment_new(int_thick, -30, 30, 1, 1, 0.0);
	spin_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_digit), 0, 0);
	g_signal_connect(spin_digit, "value-changed", G_CALLBACK(fline_digit_CB),NULL);
	
	delta = value_max - value_min;
	sprintf(min_text, "Min(%1.2e): ", value_min);
	sprintf(max_text, "Max(%1.2e): ", value_max);

	
	kemoview_get_fline_range_min(&range_min, &i_min_digit);
	adj_min_value = gtk_adjustment_new (range_min, -9.999, 9.999, 0.1, 0.1, 0.0);
	adj_min_digit = gtk_adjustment_new (i_min_digit, -20, 20, 1, 1, 0);
	spin_range_min = gtk_spin_button_new(GTK_ADJUSTMENT(adj_min_value),0,2);
	spin_min_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_min_digit),0,0);
	g_signal_connect(spin_range_min, "value-changed", G_CALLBACK(MinValueChange_CB), NULL);
	g_signal_connect(spin_min_digit, "value-changed", G_CALLBACK(MinDigitChange_CB), NULL);

	kemoview_get_fline_range_max(&range_max, &i_max_digit);
	adj_max_value = gtk_adjustment_new (range_max, -9.999, 9.999, 0.1, 0.1, 0.0);
	adj_max_digit = gtk_adjustment_new (i_max_digit, -20, 20, 1, 1, 0);
	spin_range_max = gtk_spin_button_new(GTK_ADJUSTMENT(adj_max_value),0,2);
	spin_max_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_max_digit),0,0);
	g_signal_connect(spin_range_max, "value-changed", G_CALLBACK(MaxValueChange_CB), NULL);
	g_signal_connect(spin_max_digit, "value-changed", G_CALLBACK(MaxDigitChange_CB), NULL);
	
	
	hbox_color = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_color), gtk_label_new("Color mode: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_color), combobox_color, FALSE, FALSE, 0);
	
	hbox_tube = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_tube), gtk_label_new("Draw tube: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_tube), switch_tube, FALSE, FALSE, 0);
	
	hbox_thickness = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_thickness), gtk_label_new("Thickness: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_thickness), spin_thick, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_thickness), gtk_label_new("X 10^"), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_thickness), spin_digit, TRUE, TRUE, 0);
	
	hbox_range_min = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_range_min), gtk_label_new(min_text), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_min), spin_range_min, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_min), gtk_label_new("X 10^"), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_min), spin_min_digit, TRUE, TRUE, 0);
	
	hbox_range_max = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_range_max), gtk_label_new(max_text), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_max), spin_range_max, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_max), gtk_label_new("X 10^"), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_max), spin_max_digit, TRUE, TRUE, 0);
	
	
	add_fline_draw_field_box(box_out);
	add_fline_draw_component_box(box_out);
	gtk_box_pack_start(GTK_BOX(box_out), hbox_color, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box_out), hbox_tube, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box_out), hbox_thickness, TRUE, TRUE, 0);
	
	gtk_box_pack_start(GTK_BOX(box_out), gtk_label_new("Range"), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(box_out), hbox_range_min, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(box_out), hbox_range_max, TRUE, TRUE, 0);
	return;
}
