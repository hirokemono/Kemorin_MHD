/*
 *  kemoview_gtk_axis_prefs_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#include "kemoview_gtk_axis_prefs_menu.h"

static void set_coasttube_CB(GtkComboBox *combobox_coasttube, gpointer data)
{
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) data;
    int index_mode = gtk_selected_combobox_index(combobox_coasttube);
    
    kemoview_set_view_integer(COASTLINE_TUBE, index_mode,
                              kemo_gl->kemoview_data);
    draw_full_gl(kemo_gl);
    return;
};


static void set_axisposition_CB(GtkComboBox *combobox_axispos, gpointer data)
{
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) data;
    int index_mode = gtk_selected_combobox_index(combobox_axispos);
    
    kemoview_set_object_property_flags(AXIS_POSITION, index_mode,
                                       kemo_gl->kemoview_data);
    draw_full_gl(kemo_gl);
    return;
};

GtkWidget * init_coastline_pref_menu(struct kemoviewer_gl_type *kemo_gl){
    GtkWidget * label_tree_coasttube = create_fixed_label_w_index_tree();
    GtkTreeModel * model_coasttube = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_coasttube));
    GtkTreeModel * child_model_coasttube = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_coasttube));
    int index = 0;
    index = append_ci_item_to_tree(index, "Tube",
                                   ON, child_model_coasttube);
    index = append_ci_item_to_tree(index, "Line",
                                   OFF, child_model_coasttube);

    GtkWidget *combobox_coasttube = gtk_combo_box_new_with_model(child_model_coasttube);
    GtkCellRenderer *renderer_coasttube = gtk_cell_renderer_text_new();
    if(kemoview_get_view_integer(kemo_gl->kemoview_data, COASTLINE_TUBE) == ON){
        gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_coasttube), 0);
    } else {
        gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_coasttube), 1);
    };
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_coasttube),
                               renderer_coasttube, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_coasttube),
                                   renderer_coasttube, "text",
                                   COLUMN_FIELD_NAME, NULL);
    g_signal_connect(G_OBJECT(combobox_coasttube), "changed",
                G_CALLBACK(set_coasttube_CB), (gpointer) kemo_gl);
    
    GtkWidget *hbox_coasttube = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_coasttube), gtk_label_new("Coastline type: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_coasttube), combobox_coasttube, FALSE, FALSE, 0);
    return wrap_into_frame_gtk("Coastline type", hbox_coasttube);
}


static void axis_thickness_CB(GtkWidget *entry, gpointer user_data)
{
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) user_data;
    double current_thick;
    int current_digit;
    
    double thick_in = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
    if(thick_in < 0) return;
    
    kemoview_get_axis_thickness_w_exp(kemo_gl->kemoview_data,
                                 &current_thick, &current_digit);
    kemoview_set_axis_thickness_w_exp(thick_in, current_digit,
                                       kemo_gl->kemoview_data);
    draw_full_gl(kemo_gl);
}
static void axis_digit_CB(GtkWidget *entry, gpointer user_data)
{
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) user_data;
    double current_thick;
    int current_digit;
    
    int in_digit = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
    kemoview_get_axis_thickness_w_exp(kemo_gl->kemoview_data,
                                      &current_thick, &current_digit);
    kemoview_set_axis_thickness_w_exp(current_thick, in_digit,
                                      kemo_gl->kemoview_data);
    draw_full_gl(kemo_gl);
}

GtkWidget * init_axis_position_menu(struct kemoviewer_gl_type *kemo_gl){
    GtkWidget * label_tree_axispos = create_fixed_label_w_index_tree();
    GtkTreeModel * model_axispos
            = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_axispos));
    GtkTreeModel * child_model_axispos
            = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_axispos));
    int index = 0;
    index = append_ci_item_to_tree(index, "Center",
                                   OFF, child_model_axispos);
    index = append_ci_item_to_tree(index, "Lower left",
                                   ON, child_model_axispos);

    GtkWidget *combobox_axispos
            = gtk_combo_box_new_with_model(child_model_axispos);
    GtkCellRenderer *renderer_axisposition = gtk_cell_renderer_text_new();
    if(kemoview_get_object_property_flags(kemo_gl->kemoview_data,
                                          AXIS_POSITION) == ON){
        gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_axispos), 1);
    } else {
        gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_axispos), 0);
    };
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_axispos),
                               renderer_axisposition, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_axispos),
                                   renderer_axisposition, "text",
                                   COLUMN_FIELD_NAME, NULL);
    g_signal_connect(G_OBJECT(combobox_axispos), "changed",
                G_CALLBACK(set_axisposition_CB), (gpointer) kemo_gl);
    
    
    int int_thick, current_digit;
    double current_thick;
    GtkAdjustment *adj_thick = gtk_adjustment_new(1, 0, 9, 1, 1, 0);
    GtkAdjustment *adj_digit = gtk_adjustment_new(-3, -30, 30, 1, 1, 0.0);
    GtkWidget *spin_axis_thick = gtk_spin_button_new(GTK_ADJUSTMENT(adj_thick), 0, 0);
    GtkWidget *spin_axis_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_digit), 0, 0);
    g_signal_connect(spin_axis_thick, "value-changed",
                     G_CALLBACK(axis_thickness_CB), (gpointer) kemo_gl);
    g_signal_connect(spin_axis_digit, "value-changed",
                     G_CALLBACK(axis_digit_CB), (gpointer) kemo_gl);
 
    kemoview_get_axis_thickness_w_exp(kemo_gl->kemoview_data,
                                      &current_thick, &current_digit);
    int_thick = (int) current_thick;
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_axis_thick), (double) int_thick);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_axis_digit), (double) current_digit);

    
    GtkWidget *hbox_axispos = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_axispos), gtk_label_new("Axis position: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_axispos), combobox_axispos, FALSE, FALSE, 0);
    
    GtkWidget *hbox_axiswidth = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_axiswidth), gtk_label_new("Thickness: "), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_axiswidth), spin_axis_thick, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_axiswidth), gtk_label_new("X 10^"), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_axiswidth), spin_axis_digit, TRUE, TRUE, 0);
    
    GtkWidget *axis_vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
    gtk_box_pack_start(GTK_BOX(axis_vbox), hbox_axispos, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(axis_vbox), hbox_axiswidth, TRUE, TRUE, 0);

    return wrap_into_frame_gtk("Axis parameters", axis_vbox);
}

