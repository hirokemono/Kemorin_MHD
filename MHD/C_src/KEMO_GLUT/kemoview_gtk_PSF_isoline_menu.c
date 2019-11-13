/*
 *  kemoview_gtk_PSF_isoline_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_PSF_isoline_menu.h"

static void psf_grid_switch_CB(GObject *switch_1, GParamSpec *pspec, gpointer user_data){
	kemoview_select_PSF_draw_switch(PSFGRID_TOGGLE);
	
	draw_full();
	return;
};
static void psf_zero_switch_CB(GObject *switch_1, GParamSpec *pspec, gpointer user_data){
	GtkWidget *window = GTK_WIDGET(user_data);
	kemoview_select_PSF_draw_switch(ZEROGRID_TOGGLE);
	
	draw_full();
	gtk_widget_queue_draw(window);
	return;
};

static void psf_surf_colormode_CB(GtkComboBox *combobox_gdcolor, gpointer user_data)
{
    int index_mode = gtk_selected_combobox_index(combobox_gdcolor);
	
	if (index_mode == RAINBOW_PSF_LINE)    {kemoview_set_PSF_color_param(PSFGRID_TOGGLE, RAINBOW_LINE);}
	else if (index_mode == WHITE_PSF_LINE) {kemoview_set_PSF_color_param(PSFGRID_TOGGLE, WHITE_LINE);}
    else if (index_mode == BLACK_PSF_LINE) {kemoview_set_PSF_color_param(PSFGRID_TOGGLE, BLACK_LINE);}
	
	draw_full();
	return;
};

static void set_nline_CB(GtkWidget *entry, gpointer user_data)
{
	int gtk_intvalue = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	kemoview_set_PSF_color_param(ISET_NLINE, gtk_intvalue);
	
	draw_full();
	return;
}

static void set_width_CB(GtkWidget *entry, gpointer user_data)
{
	double current_width;
	int i_digit;
	double gtk_value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_get_each_PSF_color_w_exp(ISET_WIDTH, &current_width, &i_digit);
	kemoview_set_each_PSF_color_w_exp(ISET_WIDTH, gtk_value, i_digit);
	
	draw_full();
	return;
}

static void set_digit_CB(GtkWidget *entry, gpointer user_data)
{
	double current_width;
	int i_digit;
	int gtk_value = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	kemoview_get_each_PSF_color_w_exp(ISET_WIDTH, &current_width, &i_digit);
	kemoview_set_each_PSF_color_w_exp(ISET_WIDTH, current_width, gtk_value);
	
	draw_full();
	return;
}
	
void set_gtk_isoline_menu_values(struct gtk_psf_isoline_menu *gtk_psf_isoline){
	double current_width;
	int i_digit, iflag_sfcolor;

	int current_nline = kemoview_get_PSF_color_param(ISET_NLINE);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(gtk_psf_isoline->spin_nline), (double) current_nline);

	kemoview_get_each_PSF_color_w_exp(ISET_WIDTH, &current_width, &i_digit);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(gtk_psf_isoline->spin_width), current_width);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(gtk_psf_isoline->spin_digit), (double) i_digit);

	if(kemoview_get_PSF_draw_flags(PSFGRID_TOGGLE) == 0){
		gtk_switch_set_active(GTK_SWITCH(gtk_psf_isoline->switch_1), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(gtk_psf_isoline->switch_1), TRUE);
	};
	if(kemoview_get_PSF_draw_flags(ZEROGRID_TOGGLE) == 0){
		gtk_switch_set_active(GTK_SWITCH(gtk_psf_isoline->switch_zero), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(gtk_psf_isoline->switch_zero), TRUE);
	};

	iflag_sfcolor = kemoview_get_PSF_color_param(PSFGRID_TOGGLE);
	if(iflag_sfcolor == BLACK_LINE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(gtk_psf_isoline->combobox_gdcolor), 2);
	} else 	if(iflag_sfcolor == WHITE_LINE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(gtk_psf_isoline->combobox_gdcolor), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(gtk_psf_isoline->combobox_gdcolor), 0);
	};
};

void add_gtk_isoline_menu(GtkWidget *window, struct gtk_psf_isoline_menu *gtk_psf_isoline,
							  GtkWidget *box){
	GtkWidget *hbox_draw, *hbox_zero, *hbox_color;
	GtkWidget *hbox_nline, *hbox_width;
	
	GtkWidget *vbox_iso;
	
	GtkAdjustment *adj_nline, *adj_width, *adj_digit;
	
	int index = 0;
	
	
	gtk_psf_isoline->switch_1 = gtk_switch_new();
	gtk_switch_set_active(GTK_SWITCH(gtk_psf_isoline->switch_1), FALSE);
	g_signal_connect(G_OBJECT(gtk_psf_isoline->switch_1), "notify::active",
				G_CALLBACK(psf_grid_switch_CB), (gpointer) window);
	
	gtk_psf_isoline->switch_zero = gtk_switch_new();
	gtk_switch_set_active(GTK_SWITCH(gtk_psf_isoline->switch_zero), FALSE);
	g_signal_connect(G_OBJECT(gtk_psf_isoline->switch_zero), "notify::active",
				G_CALLBACK(psf_zero_switch_CB), (gpointer) window);
	
	
	gtk_psf_isoline->label_tree_gdcolor = create_fixed_label_w_index_tree();
	gtk_psf_isoline->model_gdcolor = gtk_tree_view_get_model(GTK_TREE_VIEW(gtk_psf_isoline->label_tree_gdcolor));  
	gtk_psf_isoline->child_model_gdcolor = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(gtk_psf_isoline->model_gdcolor));
	index = 0;
	index = append_ci_item_to_tree(index, "Color lines", RAINBOW_PSF_LINE, gtk_psf_isoline->child_model_gdcolor);
	index = append_ci_item_to_tree(index, "White lines", WHITE_PSF_LINE, gtk_psf_isoline->child_model_gdcolor);
	index = append_ci_item_to_tree(index, "Black lines", BLACK_PSF_LINE, gtk_psf_isoline->child_model_gdcolor);
	
	gtk_psf_isoline->combobox_gdcolor = gtk_combo_box_new_with_model(gtk_psf_isoline->child_model_gdcolor);
	gtk_psf_isoline->renderer_gdcolor = gtk_cell_renderer_text_new();
	gtk_combo_box_set_active(GTK_COMBO_BOX(gtk_psf_isoline->combobox_gdcolor), 0);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(gtk_psf_isoline->combobox_gdcolor), gtk_psf_isoline->renderer_gdcolor, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(gtk_psf_isoline->combobox_gdcolor), gtk_psf_isoline->renderer_gdcolor,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(gtk_psf_isoline->combobox_gdcolor), "changed", 
				G_CALLBACK(psf_surf_colormode_CB), (gpointer) window);
	
	
	adj_nline = gtk_adjustment_new (10, 0, 200, 1, 1, 0.0);
	gtk_psf_isoline->spin_nline = gtk_spin_button_new(GTK_ADJUSTMENT(adj_nline), 0, 0);
	g_signal_connect(gtk_psf_isoline->spin_nline, "value-changed", G_CALLBACK(set_nline_CB), NULL);
	
	adj_width = gtk_adjustment_new(1, 1, 9, 1, 1, 0);
	gtk_psf_isoline->spin_width = gtk_spin_button_new(GTK_ADJUSTMENT(adj_width), 0, 0);
	g_signal_connect(gtk_psf_isoline->spin_width, "value-changed", G_CALLBACK(set_width_CB), NULL);
	
	adj_digit = gtk_adjustment_new(0, -10, 10, 1, 1, 0);
	gtk_psf_isoline->spin_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_digit), 0, 0);
	g_signal_connect(gtk_psf_isoline->spin_digit, "value-changed", G_CALLBACK(set_digit_CB), NULL);
	set_gtk_isoline_menu_values(gtk_psf_isoline);
	
	hbox_draw = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_draw), gtk_label_new("Draw isolines: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_draw), gtk_psf_isoline->switch_1, FALSE, FALSE, 0);
	
	hbox_zero = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_zero), gtk_label_new("Draw zero line: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_zero), gtk_psf_isoline->switch_zero, FALSE, FALSE, 0);
	
	hbox_color = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_color), gtk_label_new("Color: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_color), gtk_psf_isoline->combobox_gdcolor, FALSE, FALSE, 0);
	
	hbox_nline = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_nline), gtk_label_new("Num. of lines: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_nline), gtk_psf_isoline->spin_nline, FALSE, FALSE, 0);
	
	hbox_width = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_width), gtk_label_new("Line Width: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_width), gtk_psf_isoline->spin_width, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_width), gtk_label_new("X 10^"), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_width), gtk_psf_isoline->spin_digit, FALSE, FALSE, 0);
	
	vbox_iso = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_box_pack_start(GTK_BOX(vbox_iso), hbox_draw, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_iso), hbox_zero, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_iso), hbox_color, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_iso), hbox_nline, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_iso), hbox_width, FALSE, TRUE, 0);
	
	wrap_into_expanded_frame_gtk("Isolines", 425, 220, vbox_iso, box);
	return;
}
