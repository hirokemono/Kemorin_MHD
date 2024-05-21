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
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    int index_mode = gtk_selected_combobox_index(combobox_coasttube);
    
    kemoview_set_view_integer(COASTLINE_TUBE, index_mode, kemo_sgl);
    draw_full(kemo_sgl);
    return;
};


static void set_axisposition_CB(GtkComboBox *combobox_axispos, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    int index_mode = gtk_selected_combobox_index(combobox_axispos);
    
    kemoview_set_object_property_flags(AXIS_POSITION, index_mode, kemo_sgl);
    draw_full(kemo_sgl);
    return;
};

GtkWidget * init_coastline_pref_menu(struct kemoviewer_type *kemo_sgl){
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
    if(kemoview_get_view_integer(kemo_sgl, COASTLINE_TUBE) == ON){
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
                G_CALLBACK(set_coasttube_CB), (gpointer) kemo_sgl);
    
    GtkWidget *hbox_coasttube = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_coasttube), gtk_label_new("Coastline type: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_coasttube), combobox_coasttube, FALSE, FALSE, 0);
    return wrap_into_frame_gtk("Coastline type", hbox_coasttube);
}

GtkWidget * init_axis_position_menu(struct kemoviewer_type *kemo_sgl){
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
    if(kemoview_get_object_property_flags(kemo_sgl, AXIS_POSITION) == ON){
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
                G_CALLBACK(set_axisposition_CB), (gpointer) kemo_sgl);
    
        GtkWidget *hbox_axispos = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_axispos), gtk_label_new("Axis position: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_axispos), combobox_axispos, FALSE, FALSE, 0);
    
    return wrap_into_frame_gtk("Axis parameters", hbox_axispos);
}

