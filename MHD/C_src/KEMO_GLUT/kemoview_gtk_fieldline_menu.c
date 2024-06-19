/*
 *  kemoview_gtk_fieldline_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_fieldline_menu.h"

static void fline_thickness_CB(GtkWidget *entry, gpointer user_data)
{
    struct fieldline_gtk_menu *fline_gmenu
            = (struct fieldline_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "fline_view");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
	double current_thick;
	int current_digit;
    
	double thick_in = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	if(thick_in < 0) return;
	
    kemoview_get_VIZ_color_w_exp(kemo_gl->kemoview_data,
                                 fline_gmenu->iflag_flinemode, ISET_WIDTH,
                                 &current_thick, &current_digit);
    kemoview_set_VIZ_color_value_w_exp(fline_gmenu->iflag_flinemode, ISET_WIDTH,
                                       thick_in, current_digit,
                                       kemo_gl->kemoview_data);
    
    
    draw_full_gl(kemo_gl);
}
static void fline_digit_CB(GtkWidget *entry, gpointer user_data)
{
    struct fieldline_gtk_menu *fline_gmenu
            = (struct fieldline_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "fline_view");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
	double current_thick;
	int current_digit;
    
	int in_digit = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
    kemoview_get_VIZ_color_w_exp(kemo_gl->kemoview_data,
                                 fline_gmenu->iflag_flinemode, ISET_WIDTH,
                                 &current_thick, &current_digit);
    kemoview_set_VIZ_color_value_w_exp(fline_gmenu->iflag_flinemode, ISET_WIDTH,
                                       current_thick, in_digit,
                                       kemo_gl->kemoview_data);
	
    draw_full_gl(kemo_gl);
}

static void MinValueChange_CB(GtkWidget *entry, gpointer user_data)
{
    struct fieldline_gtk_menu *fline_gmenu
            = (struct fieldline_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "fline_view");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
	double gtk_floatvalue = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	int i_min_digit, i_max_digit;
	double minvalue, maxvalue;
    kemoview_get_VIZ_color_w_exp(kemo_gl->kemoview_data,
                                 fline_gmenu->iflag_flinemode,
                                 ISET_COLOR_MIN,
                                 &minvalue, &i_min_digit);
    kemoview_get_VIZ_color_w_exp(kemo_gl->kemoview_data,
                                 fline_gmenu->iflag_flinemode,
                                 ISET_COLOR_MAX,
                                 &maxvalue, &i_max_digit);
    kemoview_set_linear_colormap(gtk_floatvalue, i_min_digit,
                                 maxvalue, i_max_digit,
                                 fline_gmenu->iflag_flinemode,
                                 kemo_gl->kemoview_data);
	
    draw_full_gl(kemo_gl);
}

static void MinDigitChange_CB(GtkWidget *entry, gpointer user_data)
{
    struct fieldline_gtk_menu *fline_gmenu
            = (struct fieldline_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "fline_view");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
	int gtk_intvalue = (double) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	int i_min_digit, i_max_digit;
	double minvalue, maxvalue;
    kemoview_get_VIZ_color_w_exp(kemo_gl->kemoview_data,
                                 fline_gmenu->iflag_flinemode,
                                 ISET_COLOR_MIN,
                                 &minvalue, &i_min_digit);
    kemoview_get_VIZ_color_w_exp(kemo_gl->kemoview_data,
                                 fline_gmenu->iflag_flinemode,
                                 ISET_COLOR_MAX,
                                 &maxvalue, &i_max_digit);
    kemoview_set_linear_colormap(minvalue, gtk_intvalue,
                                 maxvalue, i_max_digit,
                                 fline_gmenu->iflag_flinemode,
                                 kemo_gl->kemoview_data);
	
    draw_full_gl(kemo_gl);
}

static void MaxValueChange_CB(GtkWidget *entry, gpointer user_data)
{
    struct fieldline_gtk_menu *fline_gmenu
            = (struct fieldline_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "fline_view");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
	double gtk_floatvalue = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	int i_min_digit, i_max_digit;
	double minvalue, maxvalue;
    kemoview_get_VIZ_color_w_exp(kemo_gl->kemoview_data,
                                 fline_gmenu->iflag_flinemode, 
                                 ISET_COLOR_MIN,
                                 &minvalue, &i_min_digit);
    kemoview_get_VIZ_color_w_exp(kemo_gl->kemoview_data,
                                 fline_gmenu->iflag_flinemode,
                                 ISET_COLOR_MAX,
                                 &maxvalue, &i_max_digit);
    kemoview_set_linear_colormap(minvalue, i_min_digit,
                                 gtk_floatvalue, i_max_digit,
                                 fline_gmenu->iflag_flinemode,
                                 kemo_gl->kemoview_data);
	
    draw_full_gl(kemo_gl);
}

static void MaxDigitChange_CB(GtkWidget *entry, gpointer user_data)
{
    struct fieldline_gtk_menu *fline_gmenu
            = (struct fieldline_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "fline_view");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
	int gtk_intvalue = (double) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	int i_min_digit, i_max_digit;
	double minvalue, maxvalue;
    kemoview_get_VIZ_color_w_exp(kemo_gl->kemoview_data,
                                 fline_gmenu->iflag_flinemode,
                                 ISET_COLOR_MIN,
                                 &minvalue, &i_min_digit);
    kemoview_get_VIZ_color_w_exp(kemo_gl->kemoview_data,
                                 fline_gmenu->iflag_flinemode,
                                 ISET_COLOR_MAX,
                                 &maxvalue, &i_max_digit);
    kemoview_set_linear_colormap(minvalue, i_min_digit,
                                 maxvalue, gtk_intvalue,
                                 fline_gmenu->iflag_flinemode,
                                 kemo_gl->kemoview_data);
	
    draw_full_gl(kemo_gl);
}

static void psf_fieldtube_switch_CB(GObject *switch_1, GParamSpec *pspec, gpointer user_data){
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) user_data;
    int istate = gtk_switch_get_state(GTK_SWITCH(switch_1));
    kemoview_set_line_type_flag(istate, kemo_gl->kemoview_data);
    draw_full_gl(kemo_gl);
	return;
};

static void fline_colorbar_switch_CB(GObject *switch_bar, GParamSpec *pspec, gpointer user_data){
    struct fieldline_gtk_menu *fline_gmenu
            = (struct fieldline_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "fline_view");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
    int iflag = gtk_switch_get_state(GTK_SWITCH(switch_bar));
    kemoview_set_colorbar_draw_flag(iflag, fline_gmenu->iflag_flinemode,
                                    kemo_gl->kemoview_data);
    draw_full_gl(kemo_gl);
    return;
};


static void psf_fline_colormode_CB(GtkComboBox *combobox_sfcolor, gpointer user_data)
{
    struct fieldline_gtk_menu *fline_gmenu
            = (struct fieldline_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "fline_view");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
    int index_mode = gtk_selected_combobox_index(combobox_sfcolor);
	
    kemoview_set_VIZ_patch_color_mode(index_mode,
                                      fline_gmenu->iflag_flinemode, 
                                      kemo_gl->kemoview_data);
    draw_full_gl(kemo_gl);
	return;
};





static void set_ref_vector_CB(GtkWidget *entry, gpointer user_data)
{
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) user_data;
	double gtk_floatvalue = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	int i_digit;
	double current_value;
    kemoview_get_VIZ_vector_w_exp(kemo_gl->kemoview_data,
                                  TRACER_RENDERING, ISET_PSF_REFVECT,
                                  &current_value, &i_digit);
    kemoview_set_each_VIZ_vector_w_exp(ISET_PSF_REFVECT,
                                       gtk_floatvalue, i_digit,
                                       TRACER_RENDERING,
                                       kemo_gl->kemoview_data);
    draw_full_gl(kemo_gl);
	return;
}

static void set_ref_digit_CB(GtkWidget *entry, gpointer user_data)
{
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) user_data;
	double gtk_intvalue = (double) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	int i_digit;
	double current_value;
    kemoview_get_VIZ_vector_w_exp(kemo_gl->kemoview_data,
                                  TRACER_RENDERING,
                                  ISET_PSF_REFVECT,
                                  &current_value, &i_digit);
    kemoview_set_each_VIZ_vector_w_exp(ISET_PSF_REFVECT,
                                      current_value, gtk_intvalue,
                                      TRACER_RENDERING,
                                      kemo_gl->kemoview_data);
    draw_full_gl(kemo_gl);
	return;
}

static void set_vect_increment_CB(GtkWidget *entry, gpointer user_data)
{
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) user_data;
	double gtk_floatvalue = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	int i_digit;
	double current_value;
    kemoview_get_VIZ_vector_w_exp(kemo_gl->kemoview_data,
                                  TRACER_RENDERING,
                                  ISET_VECTOR_INC,
                                  &current_value, &i_digit);
    kemoview_set_each_VIZ_vector_w_exp(ISET_VECTOR_INC,
                                      gtk_floatvalue, i_digit,
                                      TRACER_RENDERING,
                                      kemo_gl->kemoview_data);
    draw_full_gl(kemo_gl);
	return;
}

static void set_increment_digit_CB(GtkWidget *entry, gpointer user_data)
{
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) user_data;
	int gtk_intvalue = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	int i_digit;
	double current_value;
    kemoview_get_VIZ_vector_w_exp(kemo_gl->kemoview_data,
                                  TRACER_RENDERING,
                                  ISET_VECTOR_INC,
                                  &current_value, &i_digit);
    kemoview_set_each_VIZ_vector_w_exp(ISET_VECTOR_INC,
                                      current_value, gtk_intvalue,
                                      TRACER_RENDERING,
                                      kemo_gl->kemoview_data);
    draw_full_gl(kemo_gl);
	return;
}


static void psf_vector_switch_CB(GObject *switch_vect, GParamSpec *pspec, gpointer user_data){
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) user_data;
    int iflag = gtk_switch_get_state(GTK_SWITCH(switch_vect));
    kemoview_set_VIZ_vector_draw_flags(iflag, TRACER_RENDERING,
                                       kemo_gl->kemoview_data);

    draw_full_gl(kemo_gl);
	return;
};




void set_gtk_fieldline_menu(struct kemoviewer_gl_type *kemo_gl,
                            struct fieldline_gtk_menu *fline_gmenu){
	char min_text[40], max_text[40];
	double range_min, range_max;
	int i_min_digit, i_max_digit;
	double current_thick;
	int int_thick, current_digit;

    int icolor_mode = kemoview_get_VIZ_patch_color_mode(kemo_gl->kemoview_data,
                                                        fline_gmenu->iflag_flinemode);
	
	int icomp =   kemoview_get_VIZ_field_param(kemo_gl->kemoview_data,
                                               fline_gmenu->iflag_flinemode,
                                               DRAW_ADDRESS_FLAG);
	double value_min = kemoview_get_VIZ_data_range(kemo_gl->kemoview_data,
                                                   fline_gmenu->iflag_flinemode,
                                                   ISET_COLOR_MIN, icomp);
	double value_max = kemoview_get_VIZ_data_range(kemo_gl->kemoview_data,
                                                   fline_gmenu->iflag_flinemode,
                                                   ISET_COLOR_MAX, icomp);
	sprintf(min_text, "Min(%1.2e): ", value_min);
	sprintf(max_text, "Max(%1.2e): ", value_max);
	
	gtk_label_set_text(GTK_LABEL(fline_gmenu->label_min), min_text);
	gtk_label_set_text(GTK_LABEL(fline_gmenu->label_max), max_text);
	
	if(icolor_mode == BLACK_LINE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(fline_gmenu->combobox_color), 3);
	} else 	if(icolor_mode == TWO_GRAY_LINE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(fline_gmenu->combobox_color), 2);
	} else 	if(icolor_mode == TWO_COLOR_LINE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(fline_gmenu->combobox_color), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(fline_gmenu->combobox_color), 0);
	};
	
    kemoview_get_VIZ_color_w_exp(kemo_gl->kemoview_data,
                                 fline_gmenu->iflag_flinemode, ISET_WIDTH,
                                 &current_thick, &current_digit);
	int_thick = (int) current_thick;
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(fline_gmenu->spin_thick), (double) int_thick);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(fline_gmenu->spin_digit), (double) current_digit);
	
    kemoview_get_VIZ_color_w_exp(kemo_gl->kemoview_data,
                                 fline_gmenu->iflag_flinemode, ISET_COLOR_MIN,
                                 &range_min, &i_min_digit);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(fline_gmenu->spin_range_min), range_min);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(fline_gmenu->spin_min_digit), (double) i_min_digit);
	
    kemoview_get_VIZ_color_w_exp(kemo_gl->kemoview_data,
                                 fline_gmenu->iflag_flinemode, ISET_COLOR_MAX,
                                 &range_max, &i_max_digit);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(fline_gmenu->spin_range_max), range_max);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(fline_gmenu->spin_max_digit), (double) i_max_digit);
	return;
};

void init_fieldline_menu_hbox(struct kemoviewer_gl_type *kemo_gl,
                              struct fieldline_gtk_menu *fline_gmenu){
    GtkWidget *label_tree_color;
    GtkCellRenderer *renderer_color;
    GtkTreeModel *model_color;
    GtkTreeModel *child_model_color;
    int iflag = 0;
    int index = 0;

    GtkAdjustment *adj_min_value, *adj_min_digit;
    GtkAdjustment *adj_max_value, *adj_max_digit;
    
    fline_gmenu->iflag_flinemode = FIELDLINE_RENDERING;
    GtkWidget *dummy_entry = gtk_entry_new();
    g_object_set_data(G_OBJECT(dummy_entry),
                      "fline_view", (gpointer) fline_gmenu);
    g_object_set_data(G_OBJECT(dummy_entry),
                      "kemoview_gl", (gpointer) kemo_gl);
    
    fline_gmenu->label_min = gtk_label_new("Min:");
    fline_gmenu->label_max = gtk_label_new("Max:");
    
    label_tree_color = create_fixed_label_w_index_tree();
    model_color = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_color));
    child_model_color = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_color));
    index = 0;
    index = append_ci_item_to_tree(index, "Rainbow lines", RAINBOW_LINE, child_model_color);
    index = append_ci_item_to_tree(index, "Two colord linees", TWO_COLOR_LINE, child_model_color);
    index = append_ci_item_to_tree(index, "Two grayscale linees", TWO_GRAY_LINE, child_model_color);
    index = append_ci_item_to_tree(index, "Black lines", BLACK_LINE, child_model_color);
    
    renderer_color = gtk_cell_renderer_text_new();
    fline_gmenu->combobox_color = gtk_combo_box_new_with_model(child_model_color);
    gtk_combo_box_set_active(GTK_COMBO_BOX(fline_gmenu->combobox_color), 3);
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(fline_gmenu->combobox_color), renderer_color, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(fline_gmenu->combobox_color),
                                   renderer_color,"text", COLUMN_FIELD_NAME, NULL);
    g_signal_connect(G_OBJECT(fline_gmenu->combobox_color), "changed",
                     G_CALLBACK(psf_fline_colormode_CB), (gpointer) dummy_entry);
    
    
    fline_gmenu->fline_switch_bar = gtk_switch_new();
    iflag = kemoview_get_colorbar_draw_flag(kemo_gl->kemoview_data,
                                            fline_gmenu->iflag_flinemode);
    gtk_switch_set_state(GTK_SWITCH(fline_gmenu->fline_switch_bar), iflag);
    gtk_switch_set_active(GTK_SWITCH(fline_gmenu->fline_switch_bar), FALSE);
    g_signal_connect(G_OBJECT(fline_gmenu->fline_switch_bar), "notify::active",
                     G_CALLBACK(fline_colorbar_switch_CB), (gpointer) dummy_entry);

    if(kemoview_get_colorbar_draw_flag(kemo_gl->kemoview_data,
                                       fline_gmenu->iflag_flinemode) == 0){
        gtk_switch_set_active(GTK_SWITCH(fline_gmenu->fline_switch_bar), FALSE);
    } else {
        gtk_switch_set_active(GTK_SWITCH(fline_gmenu->fline_switch_bar), TRUE);
    };

    
    fline_gmenu->switch_tube = gtk_switch_new();
    iflag = kemoview_get_line_type_flag(kemo_gl->kemoview_data);
    gtk_switch_set_state(GTK_SWITCH(fline_gmenu->switch_tube), iflag);
    gtk_switch_set_active(GTK_SWITCH(fline_gmenu->switch_tube), FALSE);
    g_signal_connect(G_OBJECT(fline_gmenu->switch_tube), "notify::active",
                     G_CALLBACK(psf_fieldtube_switch_CB), (gpointer) kemo_gl);
    
    GtkAdjustment *adj_thick = gtk_adjustment_new(1, 0, 9, 1, 1, 0);
    GtkAdjustment *adj_digit = gtk_adjustment_new(-3, -30, 30, 1, 1, 0.0);
    fline_gmenu->spin_thick = gtk_spin_button_new(GTK_ADJUSTMENT(adj_thick), 0, 0);
    fline_gmenu->spin_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_digit), 0, 0);
    g_signal_connect(fline_gmenu->spin_thick, "value-changed",
                     G_CALLBACK(fline_thickness_CB), (gpointer) dummy_entry);
    g_signal_connect(fline_gmenu->spin_digit, "value-changed",
                     G_CALLBACK(fline_digit_CB), (gpointer) dummy_entry);
 
    adj_min_value = gtk_adjustment_new(0.0, -9.999, 9.999, 0.1, 0.1, 0.0);
    adj_min_digit = gtk_adjustment_new(0, -20, 20, 1, 1, 0);
    fline_gmenu->spin_range_min = gtk_spin_button_new(GTK_ADJUSTMENT(adj_min_value),0,2);
    fline_gmenu->spin_min_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_min_digit),0,0);
    g_signal_connect(fline_gmenu->spin_range_min, "value-changed",
                     G_CALLBACK(MinValueChange_CB), (gpointer) dummy_entry);
    g_signal_connect(fline_gmenu->spin_min_digit, "value-changed",
                     G_CALLBACK(MinDigitChange_CB), (gpointer) dummy_entry);

    adj_max_value = gtk_adjustment_new(0.0, -9.999, 9.999, 0.1, 0.1, 0.0);
    adj_max_digit = gtk_adjustment_new(0, -20, 20, 1, 1, 0);
    fline_gmenu->spin_range_max = gtk_spin_button_new(GTK_ADJUSTMENT(adj_max_value),0,2);
    fline_gmenu->spin_max_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_max_digit),0,0);
    g_signal_connect(fline_gmenu->spin_range_max, "value-changed",
                     G_CALLBACK(MaxValueChange_CB), (gpointer) dummy_entry);
    g_signal_connect(fline_gmenu->spin_max_digit, "value-changed",
                     G_CALLBACK(MaxDigitChange_CB), (gpointer) dummy_entry);
    
    init_colormap_params_4_viewer(fline_gmenu->iflag_flinemode, kemo_gl,
                                  fline_gmenu->fline_color_vws);
    fline_gmenu->expander_fline_color = init_gtk_psf_colormap_expander(kemo_gl, fline_gmenu->flineWin,
                                                                       fline_gmenu->fline_color_vws);
    return;
}

GtkWidget * pack_fieldline_menu_frame(struct fieldline_gtk_menu *fline_gmenu){
    GtkWidget *frame;
    
    GtkWidget *hbox_color = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_color), gtk_label_new("Color mode: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_color), fline_gmenu->combobox_color, FALSE, FALSE, 0);
	
    GtkWidget *hbox_tube = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_tube), gtk_label_new("Draw tube: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_tube), fline_gmenu->switch_tube, FALSE, FALSE, 0);
	
    GtkWidget *hbox_thickness = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_thickness), gtk_label_new("Thickness: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_thickness), fline_gmenu->spin_thick, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_thickness), gtk_label_new("X 10^"), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_thickness), fline_gmenu->spin_digit, TRUE, TRUE, 0);
	
    GtkWidget *hbox_range_min = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_range_min), fline_gmenu->label_min, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_min), fline_gmenu->spin_range_min, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_min), gtk_label_new("X 10^"), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_min), fline_gmenu->spin_min_digit, TRUE, TRUE, 0);
	
    GtkWidget *hbox_range_max = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_range_max), fline_gmenu->label_max, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_max), fline_gmenu->spin_range_max, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_max), gtk_label_new("X 10^"), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_max), fline_gmenu->spin_max_digit, TRUE, TRUE, 0);
	
    GtkWidget *hbox_field = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_field), gtk_label_new("Field: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_field), fline_gmenu->combobox_field, FALSE, FALSE, 0);
    
    GtkWidget *hbox_comp = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_comp), gtk_label_new("Component: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_comp), fline_gmenu->combobox_comp, FALSE, FALSE, 0);
    

    GtkWidget *hbox_bar = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_bar), gtk_label_new("Draw color bar: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_bar), fline_gmenu->fline_switch_bar, FALSE, FALSE, 0);
    

    GtkWidget *menu_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
    gtk_box_pack_start(GTK_BOX(menu_box), fline_gmenu->closeButton, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(menu_box), hbox_field, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(menu_box), hbox_comp, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(menu_box), hbox_color, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(menu_box), hbox_bar, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(menu_box), hbox_tube, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(menu_box), hbox_thickness, TRUE, TRUE, 0);

	gtk_box_pack_start(GTK_BOX(menu_box), gtk_label_new("Range"), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(menu_box), hbox_range_min, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(menu_box), hbox_range_max, TRUE, TRUE, 0);
    
    gtk_box_pack_start(GTK_BOX(menu_box), fline_gmenu->expander_fline_color, TRUE, TRUE, 0);
    frame = wrap_into_frame_gtk("Fieldline", menu_box);
    return frame;
}




void init_tracer_menu_hbox(struct kemoviewer_gl_type *kemo_gl,
                           struct fieldline_gtk_menu *fline_gmenu){
    GtkWidget *label_tree_color;
    GtkCellRenderer *renderer_color;
    GtkTreeModel *model_color;
    GtkTreeModel *child_model_color;
    int iflag = 0;
    int index = 0;
    
    fline_gmenu->iflag_flinemode = TRACER_RENDERING;
    GtkWidget *dummy_entry = gtk_entry_new();
    g_object_set_data(G_OBJECT(dummy_entry),
                      "fline_view", (gpointer) fline_gmenu);
    g_object_set_data(G_OBJECT(dummy_entry),
                      "kemoview_gl", (gpointer) kemo_gl);
    
    fline_gmenu->label_min = gtk_label_new("Min:");
    fline_gmenu->label_max = gtk_label_new("Max:");
    
    label_tree_color = create_fixed_label_w_index_tree();
    model_color = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_color));
    child_model_color = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_color));
    index = 0;
    index = append_ci_item_to_tree(index, "Rainbow lines", RAINBOW_LINE, child_model_color);
    index = append_ci_item_to_tree(index, "Two colord linees", TWO_COLOR_LINE, child_model_color);
    index = append_ci_item_to_tree(index, "Two grayscale linees", TWO_GRAY_LINE, child_model_color);
    index = append_ci_item_to_tree(index, "Black lines", BLACK_LINE, child_model_color);
    
    renderer_color = gtk_cell_renderer_text_new();
    fline_gmenu->combobox_color = gtk_combo_box_new_with_model(child_model_color);
    gtk_combo_box_set_active(GTK_COMBO_BOX(fline_gmenu->combobox_color), 3);
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(fline_gmenu->combobox_color), renderer_color, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(fline_gmenu->combobox_color),
                                   renderer_color,"text", COLUMN_FIELD_NAME, NULL);
    g_signal_connect(G_OBJECT(fline_gmenu->combobox_color), "changed",
                     G_CALLBACK(psf_fline_colormode_CB), (gpointer) dummy_entry);
    
    
    
	fline_gmenu->switch_vect = gtk_switch_new();
    iflag = kemoview_get_VIZ_vector_draw_flags(kemo_gl->kemoview_data,
                                               TRACER_RENDERING);
    gtk_switch_set_state(GTK_SWITCH(fline_gmenu->switch_vect), iflag);
	g_signal_connect(G_OBJECT(fline_gmenu->switch_vect), "notify::active",
				G_CALLBACK(psf_vector_switch_CB), (gpointer) kemo_gl);
	
	if(kemoview_get_VIZ_vector_draw_flags(kemo_gl->kemoview_data,
                                          TRACER_RENDERING) == 0){
		gtk_switch_set_active(GTK_SWITCH(fline_gmenu->switch_vect), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(fline_gmenu->switch_vect), TRUE);
	};
	
    
    fline_gmenu->fline_switch_bar = gtk_switch_new();
    iflag = kemoview_get_colorbar_draw_flag(kemo_gl->kemoview_data,
                                            fline_gmenu->iflag_flinemode);
    gtk_switch_set_state(GTK_SWITCH(fline_gmenu->fline_switch_bar), iflag);
    gtk_switch_set_active(GTK_SWITCH(fline_gmenu->fline_switch_bar), FALSE);
    g_signal_connect(G_OBJECT(fline_gmenu->fline_switch_bar), "notify::active",
                     G_CALLBACK(fline_colorbar_switch_CB), (gpointer) dummy_entry);

    if(kemoview_get_colorbar_draw_flag(kemo_gl->kemoview_data,
                                       fline_gmenu->iflag_flinemode) == 0){
        gtk_switch_set_active(GTK_SWITCH(fline_gmenu->fline_switch_bar), FALSE);
    } else {
        gtk_switch_set_active(GTK_SWITCH(fline_gmenu->fline_switch_bar), TRUE);
    };
    
    GtkAdjustment *adj_thick = gtk_adjustment_new(1, 0, 9, 1, 1, 0);
    GtkAdjustment *adj_digit = gtk_adjustment_new(-3, -30, 30, 1, 1, 0.0);
    fline_gmenu->spin_thick = gtk_spin_button_new(GTK_ADJUSTMENT(adj_thick), 0, 0);
    fline_gmenu->spin_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_digit), 0, 0);
    g_signal_connect(fline_gmenu->spin_thick, "value-changed",
                     G_CALLBACK(fline_thickness_CB), (gpointer) dummy_entry);
    g_signal_connect(fline_gmenu->spin_digit, "value-changed",
                     G_CALLBACK(fline_digit_CB), (gpointer) dummy_entry);
 
    GtkAdjustment *adj_min_value = gtk_adjustment_new(0.0, -9.999, 9.999, 0.1, 0.1, 0.0);
    GtkAdjustment *adj_min_digit = gtk_adjustment_new(0, -20, 20, 1, 1, 0);
    fline_gmenu->spin_range_min = gtk_spin_button_new(GTK_ADJUSTMENT(adj_min_value),0,2);
    fline_gmenu->spin_min_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_min_digit),0,0);
    g_signal_connect(fline_gmenu->spin_range_min, "value-changed",
                     G_CALLBACK(MinValueChange_CB), (gpointer) dummy_entry);
    g_signal_connect(fline_gmenu->spin_min_digit, "value-changed",
                     G_CALLBACK(MinDigitChange_CB), (gpointer) dummy_entry);

    GtkAdjustment *adj_max_value = gtk_adjustment_new(0.0, -9.999, 9.999, 0.1, 0.1, 0.0);
    GtkAdjustment *adj_max_digit = gtk_adjustment_new(0, -20, 20, 1, 1, 0);
    fline_gmenu->spin_range_max = gtk_spin_button_new(GTK_ADJUSTMENT(adj_max_value),0,2);
    fline_gmenu->spin_max_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_max_digit),0,0);
    g_signal_connect(fline_gmenu->spin_range_max, "value-changed",
                     G_CALLBACK(MaxValueChange_CB), (gpointer) dummy_entry);
    g_signal_connect(fline_gmenu->spin_max_digit, "value-changed",
                     G_CALLBACK(MaxDigitChange_CB), (gpointer) dummy_entry);
    
    
	GtkAdjustment *adj_ref_vect = gtk_adjustment_new(1, 1, 9, 1, 1, 0);
	fline_gmenu->spin_ref_vect = gtk_spin_button_new(GTK_ADJUSTMENT(adj_ref_vect), 0, 0);
	g_signal_connect(fline_gmenu->spin_ref_vect, "value-changed", 
                     G_CALLBACK(set_ref_vector_CB), (gpointer) kemo_gl);
	
	GtkAdjustment *adj_ref_digit = gtk_adjustment_new(0, -10, 10, 1, 1, 0);
	fline_gmenu->spin_ref_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_ref_digit), 0, 0);
	g_signal_connect(fline_gmenu->spin_ref_digit, "value-changed",
                     G_CALLBACK(set_ref_digit_CB), (gpointer) kemo_gl);
	
	GtkAdjustment *adj_vect_inc = gtk_adjustment_new(1, 1, 9, 1, 1, 0);
	fline_gmenu->spin_vect_inc = gtk_spin_button_new(GTK_ADJUSTMENT(adj_vect_inc), 0, 0);
	g_signal_connect(fline_gmenu->spin_vect_inc, "value-changed",
					 G_CALLBACK(set_vect_increment_CB), (gpointer) kemo_gl);
	
	GtkAdjustment *adj_inc_digit = gtk_adjustment_new(0, 0, 10, 1, 1, 0);
	fline_gmenu->spin_inc_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_inc_digit), 0, 0);
	g_signal_connect(fline_gmenu->spin_inc_digit, "value-changed",
					 G_CALLBACK(set_increment_digit_CB), (gpointer) kemo_gl);
	
    
    init_colormap_params_4_viewer(fline_gmenu->iflag_flinemode, kemo_gl,
                                  fline_gmenu->fline_color_vws);
    fline_gmenu->expander_fline_color = init_gtk_psf_colormap_expander(kemo_gl, fline_gmenu->flineWin,
                                                                       fline_gmenu->fline_color_vws);
    return;
}

GtkWidget * pack_tracer_menu_frame(struct fieldline_gtk_menu *fline_gmenu){
    GtkWidget *frame;
    
    GtkWidget *hbox_color = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_color), gtk_label_new("Color mode: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_color), fline_gmenu->combobox_color, FALSE, FALSE, 0);
	
    GtkWidget *hbox_thickness = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_thickness), gtk_label_new("Thickness: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_thickness), fline_gmenu->spin_thick, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_thickness), gtk_label_new("X 10^"), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_thickness), fline_gmenu->spin_digit, TRUE, TRUE, 0);
	
    GtkWidget *hbox_range_min = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_range_min), fline_gmenu->label_min, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_min), fline_gmenu->spin_range_min, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_min), gtk_label_new("X 10^"), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_min), fline_gmenu->spin_min_digit, TRUE, TRUE, 0);
	
    GtkWidget *hbox_range_max = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_range_max), fline_gmenu->label_max, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_max), fline_gmenu->spin_range_max, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_max), gtk_label_new("X 10^"), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range_max), fline_gmenu->spin_max_digit, TRUE, TRUE, 0);
	
    GtkWidget *hbox_field = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_field), gtk_label_new("Field: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_field), fline_gmenu->combobox_field, FALSE, FALSE, 0);
    
    GtkWidget *hbox_comp = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_comp), gtk_label_new("Component: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_comp), fline_gmenu->combobox_comp, FALSE, FALSE, 0);
    

    GtkWidget *hbox_bar = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_bar), gtk_label_new("Draw color bar: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_bar), fline_gmenu->fline_switch_bar, FALSE, FALSE, 0);
    
	GtkWidget *hbox_vec = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_vec), gtk_label_new("Draw vector: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_vec), fline_gmenu->switch_vect, FALSE, FALSE, 0);
	
	GtkWidget *hbox_22 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_22), gtk_label_new("Increment: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_22), fline_gmenu->spin_vect_inc, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_22), gtk_label_new("X 10^"), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_22), fline_gmenu->spin_inc_digit, TRUE, TRUE, 0);
	
	GtkWidget *hbox_12 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_12), gtk_label_new("Reference: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_12), fline_gmenu->spin_ref_vect, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_12), gtk_label_new("X 10^"), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_12), fline_gmenu->spin_ref_digit, TRUE, TRUE, 0);
    
    
    GtkWidget *menu_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
    gtk_box_pack_start(GTK_BOX(menu_box), fline_gmenu->closeButton, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(menu_box), hbox_field, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(menu_box), hbox_comp, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(menu_box), hbox_color, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(menu_box), hbox_bar, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(menu_box), hbox_thickness, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(menu_box), hbox_vec, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(menu_box), hbox_22, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(menu_box), hbox_12, TRUE, TRUE, 0);

	gtk_box_pack_start(GTK_BOX(menu_box), gtk_label_new("Range"), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(menu_box), hbox_range_min, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(menu_box), hbox_range_max, TRUE, TRUE, 0);
    
    gtk_box_pack_start(GTK_BOX(menu_box), fline_gmenu->expander_fline_color, TRUE, TRUE, 0);
    frame = wrap_into_frame_gtk("Fieldline", menu_box);
    return frame;
}
