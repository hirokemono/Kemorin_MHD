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
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    
	double thick_in = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	if(thick_in <= 0) return;
	
	kemoview_get_fline_color_w_exp(kemo_sgl, ISET_WIDTH, 
                                   &current_thick, &current_digit);
	kemoview_set_fline_color_w_exp(ISET_WIDTH, thick_in, current_digit, kemo_sgl);

    draw_full(kemo_sgl);
}
static void fline_digit_CB(GtkWidget *entry, gpointer data)
{
	double current_thick;
	int current_digit;
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    
	int in_digit = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	kemoview_get_fline_color_w_exp(kemo_sgl, ISET_WIDTH,
                                   &current_thick, &current_digit);
	kemoview_set_fline_color_w_exp(ISET_WIDTH, current_thick, in_digit, kemo_sgl);
	
    draw_full(kemo_sgl);
}

static void MinValueChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	double gtk_floatvalue = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	int i_min_digit, i_max_digit;
	double minvalue, maxvalue;
	kemoview_get_fline_color_w_exp(kemo_sgl, ISET_COLOR_MIN,
                                   &minvalue, &i_min_digit);
	kemoview_get_fline_color_w_exp(kemo_sgl, ISET_COLOR_MAX, 
                                   &maxvalue, &i_max_digit);
	kemoview_set_fline_linear_colormap(gtk_floatvalue, i_min_digit, 
									   maxvalue, i_max_digit, kemo_sgl);
	
    draw_full(kemo_sgl);
}

static void MinDigitChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	int gtk_intvalue = (double) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	int i_min_digit, i_max_digit;
	double minvalue, maxvalue;
	kemoview_get_fline_color_w_exp(kemo_sgl, ISET_COLOR_MIN,
                                   &minvalue, &i_min_digit);
	kemoview_get_fline_color_w_exp(kemo_sgl, ISET_COLOR_MAX,
                                   &maxvalue, &i_max_digit);
	kemoview_set_fline_linear_colormap(minvalue, gtk_intvalue, 
									   maxvalue, i_max_digit, kemo_sgl);
	
    draw_full(kemo_sgl);
}

static void MaxValueChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	double gtk_floatvalue = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	int i_min_digit, i_max_digit;
	double minvalue, maxvalue;
	kemoview_get_fline_color_w_exp(kemo_sgl, ISET_COLOR_MIN,
                                   &minvalue, &i_min_digit);
	kemoview_get_fline_color_w_exp(kemo_sgl, ISET_COLOR_MAX,
                                   &maxvalue, &i_max_digit);
	kemoview_set_fline_linear_colormap(minvalue, i_min_digit, 
									   gtk_floatvalue, i_max_digit, 
                                       kemo_sgl);
	
    draw_full(kemo_sgl);
}

static void MaxDigitChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	int gtk_intvalue = (double) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	int i_min_digit, i_max_digit;
	double minvalue, maxvalue;
	kemoview_get_fline_color_w_exp(kemo_sgl, ISET_COLOR_MIN,
                                   &minvalue, &i_min_digit);
	kemoview_get_fline_color_w_exp(kemo_sgl, ISET_COLOR_MAX,
                                   &maxvalue, &i_max_digit);
	kemoview_set_fline_linear_colormap(minvalue, i_min_digit, 
									   maxvalue, gtk_intvalue,
                                       kemo_sgl);
	
    draw_full(kemo_sgl);
}

static void psf_fieldtube_switch_CB(GObject *switch_1, GParamSpec *pspec, gpointer data){
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    int istate = gtk_switch_get_state(GTK_SWITCH(switch_1));
    kemoview_set_fline_field_param(LINETYPE_FLAG, istate, kemo_sgl);
    draw_full(kemo_sgl);
	return;
};

static void psf_fline_colormode_CB(GtkComboBox *combobox_sfcolor, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    int index_mode = gtk_selected_combobox_index(combobox_sfcolor);
	
	kemoview_set_fline_color_param(ISET_COLORMAP, index_mode, kemo_sgl);
    draw_full(kemo_sgl);
	return;
};

void set_gtk_fieldline_menu(struct kemoviewer_type *kemo_sgl,
                            struct fieldline_gtk_menu *fline_menu){
	char min_text[40], max_text[40];
	double range_min, range_max;
	int i_min_digit, i_max_digit;
	double current_thick;
	int int_thick, current_digit;
	/*
	int ifield =  kemoview_get_fline_field_param(kemo_sgl, FIELD_SEL_FLAG);
	int num_fld = kemoview_get_fline_field_param(kemo_sgl, NUM_FIELD_FLAG);
	int num_comp = kemoview_get_fline_color_num_comps(kemo_sgl, ifield);
	*/
	int icolor_mode = kemoview_get_fline_color_param(kemo_sgl, ISET_COLORMAP);
	int itype_fline = kemoview_get_fline_field_param(kemo_sgl, LINETYPE_FLAG);
	
	int icomp =   kemoview_get_fline_field_param(kemo_sgl, DRAW_ADDRESS_FLAG);
	double value_min = kemoview_get_fline_data_range(kemo_sgl, ISET_COLOR_MIN, icomp);
	double value_max = kemoview_get_fline_data_range(kemo_sgl, ISET_COLOR_MAX, icomp);
	sprintf(min_text, "Min(%1.2e): ", value_min);
	sprintf(max_text, "Max(%1.2e): ", value_max);
	
	gtk_label_set_text(GTK_LABEL(fline_menu->label_min), min_text);
	gtk_label_set_text(GTK_LABEL(fline_menu->label_max), max_text);
	
	if(icolor_mode == BLACK_LINE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(fline_menu->combobox_color), 3);
	} else 	if(icolor_mode == TWO_GRAY_LINE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(fline_menu->combobox_color), 2);
	} else 	if(icolor_mode == TWO_COLOR_LINE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(fline_menu->combobox_color), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(fline_menu->combobox_color), 0);
	};
	
	if(itype_fline == 0){
		gtk_switch_set_active(GTK_SWITCH(fline_menu->switch_tube), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(fline_menu->switch_tube), TRUE);
	};
	
	kemoview_get_fline_color_w_exp(kemo_sgl, ISET_WIDTH,
                                   &current_thick, &current_digit);
	int_thick = (int) current_thick;
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(fline_menu->spin_thick), (double) int_thick);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(fline_menu->spin_digit), (double) current_digit);
	
	kemoview_get_fline_color_w_exp(kemo_sgl, ISET_COLOR_MIN,
                                   &range_min, &i_min_digit);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(fline_menu->spin_range_min), range_min);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(fline_menu->spin_min_digit), (double) i_min_digit);
	
	kemoview_get_fline_color_w_exp(kemo_sgl, ISET_COLOR_MAX,
                                   &range_max, &i_max_digit);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(fline_menu->spin_range_max), range_max);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(fline_menu->spin_max_digit), (double) i_max_digit);
	return;
};

void init_fieldline_menu_hbox(struct kemoviewer_type *kemo_sgl,
                              struct fieldline_gtk_menu *fline_menu){
    GtkWidget *label_tree_color;
    GtkCellRenderer *renderer_color;
    GtkTreeModel *model_color;
    GtkTreeModel *child_model_color;
    
    int index = 0;
    
    GtkAdjustment *adj_thick, *adj_digit;
    
    GtkAdjustment *adj_min_value, *adj_min_digit;
    GtkAdjustment *adj_max_value, *adj_max_digit;
    
    
    fline_menu->label_min = gtk_label_new("Min:");
    fline_menu->label_max = gtk_label_new("Max:");
    
    label_tree_color = create_fixed_label_w_index_tree();
    model_color = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_color));
    child_model_color = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_color));
    index = 0;
    index = append_ci_item_to_tree(index, "Rainbow lines", RAINBOW_LINE, child_model_color);
    index = append_ci_item_to_tree(index, "Two colord linees", TWO_COLOR_LINE, child_model_color);
    index = append_ci_item_to_tree(index, "Two grayscale linees", TWO_GRAY_LINE, child_model_color);
    index = append_ci_item_to_tree(index, "Black lines", BLACK_LINE, child_model_color);
    
    renderer_color = gtk_cell_renderer_text_new();
    fline_menu->combobox_color = gtk_combo_box_new_with_model(child_model_color);
    gtk_combo_box_set_active(GTK_COMBO_BOX(fline_menu->combobox_color), 3);
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(fline_menu->combobox_color), renderer_color, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(fline_menu->combobox_color),
                                   renderer_color,"text", COLUMN_FIELD_NAME, NULL);
    g_signal_connect(G_OBJECT(fline_menu->combobox_color), "changed",
                     G_CALLBACK(psf_fline_colormode_CB), (gpointer) kemo_sgl);
    
    
    fline_menu->switch_tube = gtk_switch_new();
    int iflag = get_fline_field_param(LINETYPE_FLAG, kemo_sgl);
    gtk_switch_set_state(GTK_SWITCH(fline_menu->switch_tube), iflag);
    gtk_switch_set_active(GTK_SWITCH(fline_menu->switch_tube), FALSE);
    g_signal_connect(G_OBJECT(fline_menu->switch_tube), "notify::active",
                     G_CALLBACK(psf_fieldtube_switch_CB), (gpointer) kemo_sgl);
    
    adj_thick = gtk_adjustment_new(1, 0, 9, 1, 1, 0.0);
    adj_digit = gtk_adjustment_new(-3, -30, 30, 1, 1, 0.0);
    fline_menu->spin_thick = gtk_spin_button_new(GTK_ADJUSTMENT(adj_thick), 0, 0);
    fline_menu->spin_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_digit), 0, 0);
    g_signal_connect(fline_menu->spin_thick, "value-changed",
                     G_CALLBACK(fline_thickness_CB), (gpointer) kemo_sgl);
    g_signal_connect(fline_menu->spin_digit, "value-changed",
                     G_CALLBACK(fline_digit_CB), (gpointer) kemo_sgl);
    
    adj_min_value = gtk_adjustment_new(0.0, -9.999, 9.999, 0.1, 0.1, 0.0);
    adj_min_digit = gtk_adjustment_new(0, -20, 20, 1, 1, 0);
    fline_menu->spin_range_min = gtk_spin_button_new(GTK_ADJUSTMENT(adj_min_value),0,2);
    fline_menu->spin_min_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_min_digit),0,0);
    g_signal_connect(fline_menu->spin_range_min, "value-changed",
                     G_CALLBACK(MinValueChange_CB), (gpointer) kemo_sgl);
    g_signal_connect(fline_menu->spin_min_digit, "value-changed",
                     G_CALLBACK(MinDigitChange_CB), (gpointer) kemo_sgl);

    adj_max_value = gtk_adjustment_new(0.0, -9.999, 9.999, 0.1, 0.1, 0.0);
    adj_max_digit = gtk_adjustment_new(0, -20, 20, 1, 1, 0);
    fline_menu->spin_range_max = gtk_spin_button_new(GTK_ADJUSTMENT(adj_max_value),0,2);
    fline_menu->spin_max_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_max_digit),0,0);
    g_signal_connect(fline_menu->spin_range_max, "value-changed",
                     G_CALLBACK(MaxValueChange_CB), (gpointer) kemo_sgl);
    g_signal_connect(fline_menu->spin_max_digit, "value-changed",
                     G_CALLBACK(MaxDigitChange_CB), (gpointer) kemo_sgl);
    
    fline_menu->combobox_field = fline_draw_field_box(kemo_sgl,
                                                      fline_menu->label_tree_field,
                                                      fline_menu->renderer_field);
    fline_menu->combobox_comp = fline_draw_component_combobox(kemo_sgl,
                                                              fline_menu->label_tree_comp,
                                                              fline_menu->renderer_comp);
    return;
}

GtkWidget * pack_fieldline_menu_frame(struct fieldline_gtk_menu *fline_menu){
    GtkWidget *frame;
    
    GtkWidget *hbox_color = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_color), gtk_label_new("Color mode: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_color), fline_menu->combobox_color, FALSE, FALSE, 0);
	
    GtkWidget *hbox_tube = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_tube), gtk_label_new("Draw tube: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_tube), fline_menu->switch_tube, FALSE, FALSE, 0);
	
    GtkWidget *hbox_thickness = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_thickness), gtk_label_new("Thickness: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_thickness), fline_menu->spin_thick, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_thickness), gtk_label_new("X 10^"), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_thickness), fline_menu->spin_digit, TRUE, TRUE, 0);
	
    GtkWidget *hbox_range_min = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_range_min), fline_menu->label_min, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_min), fline_menu->spin_range_min, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_min), gtk_label_new("X 10^"), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_min), fline_menu->spin_min_digit, TRUE, TRUE, 0);
	
    GtkWidget *hbox_range_max = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_range_max), fline_menu->label_max, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_max), fline_menu->spin_range_max, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_max), gtk_label_new("X 10^"), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_max), fline_menu->spin_max_digit, TRUE, TRUE, 0);
	
    GtkWidget *hbox_field = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_field), gtk_label_new("Field: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_field), fline_menu->combobox_field, FALSE, FALSE, 0);
    
    GtkWidget *hbox_comp = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_comp), gtk_label_new("Component: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_comp), fline_menu->combobox_comp, FALSE, FALSE, 0);
    

    GtkWidget *menu_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
    gtk_box_pack_start(GTK_BOX(menu_box), fline_menu->closeButton, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(menu_box), hbox_field, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(menu_box), hbox_comp, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(menu_box), hbox_color, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(menu_box), hbox_tube, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(menu_box), hbox_thickness, TRUE, TRUE, 0);
	
	gtk_box_pack_start(GTK_BOX(menu_box), gtk_label_new("Range"), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(menu_box), hbox_range_min, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(menu_box), hbox_range_max, TRUE, TRUE, 0);
    
    frame = wrap_into_frame_gtk("Fieldline", menu_box);
    return frame;
}
