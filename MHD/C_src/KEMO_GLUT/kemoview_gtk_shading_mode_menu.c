/*
 *  kemoview_gtk_shading_mode_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#include "kemoview_gtk_shading_mode_menu.h"

static void set_shading_mode_CB(GtkComboBox *combobox_shading, gpointer data)
{
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) data;
    int index_mode = gtk_selected_combobox_index(combobox_shading);
    
    kemoview_set_object_property_flags(SHADING_SWITCH, index_mode,
                                       kemo_gl->kemoview_data);
    draw_full_gl(kemo_gl);
    return;
};

static void set_surface_direction_CB(GtkComboBox *combobox_surfdir, gpointer data)
{
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) data;
    int index_mode = gtk_selected_combobox_index(combobox_surfdir);
    
    kemoview_set_object_property_flags(POLYGON_SWITCH, index_mode,
                                       kemo_gl->kemoview_data);
    draw_full_gl(kemo_gl);
    return;
};

GtkWidget * shading_mode_menu_frame(struct kemoviewer_gl_type *kemo_gl){
/* Shading mode chooser */
    
    GtkWidget * label_tree_shading = create_fixed_label_w_index_tree();
    GtkTreeModel * model_shading = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_shading));
    GtkTreeModel * child_model_shading = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_shading));
    int index = 0;
    index = append_ci_item_to_tree(index, "Smooth surface", SMOOTH_SHADE, child_model_shading);
    index = append_ci_item_to_tree(index, "Flat surface", FLAT_SHADE, child_model_shading);
    
    GtkWidget *combobox_shading = gtk_combo_box_new_with_model(child_model_shading);
    GtkCellRenderer *renderer_shading = gtk_cell_renderer_text_new();
    if(kemoview_get_object_property_flags(kemo_gl->kemoview_data, SHADING_SWITCH) == FLAT_SHADE){
        gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_shading), 1);
    } else {
        gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_shading), 0);
    };
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_shading), renderer_shading, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_shading), renderer_shading,
                "text", COLUMN_FIELD_NAME, NULL);
    g_signal_connect(G_OBJECT(combobox_shading), "changed",
                G_CALLBACK(set_shading_mode_CB), (gpointer) kemo_gl);
    
    GtkWidget *hbox_shading = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_shading), gtk_label_new("Shading mode: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_shading), combobox_shading, FALSE, FALSE, 0);
    
    
/* Surface direction chooser */
    
    GtkWidget *label_tree_surf_dir = create_fixed_label_w_index_tree();
    GtkTreeModel *model_surf_dir = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_surf_dir));
    GtkTreeModel *child_model_surf_dir = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_surf_dir));
    index = 0;
    index = append_ci_item_to_tree(index, "Normal surface", NORMAL_POLYGON, child_model_surf_dir);
    index = append_ci_item_to_tree(index, "Reverse surface", REVERSE_POLYGON, child_model_surf_dir);
    
    GtkWidget *combobox_surf_dir = gtk_combo_box_new_with_model(child_model_surf_dir);
    GtkCellRenderer *renderer_surf_dir = gtk_cell_renderer_text_new();
    if(kemoview_get_object_property_flags(kemo_gl->kemoview_data, POLYGON_SWITCH) == REVERSE_POLYGON){
        gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_surf_dir), 1);
    } else {
        gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_surf_dir), 0);
    };
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_surf_dir), renderer_surf_dir, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_surf_dir), renderer_surf_dir,
                "text", COLUMN_FIELD_NAME, NULL);
    g_signal_connect(G_OBJECT(combobox_surf_dir), "changed",
                G_CALLBACK(set_surface_direction_CB), (gpointer) kemo_gl);
    
    GtkWidget *hbox_surf_dir = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_surf_dir), gtk_label_new("Surface direction: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_surf_dir), combobox_surf_dir, FALSE, FALSE, 0);
    
    
    GtkWidget *vbox_shading = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_pack_start(GTK_BOX(vbox_shading), hbox_shading, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_shading), hbox_surf_dir, TRUE, TRUE, 0);
    
    return wrap_into_frame_gtk("Shading parameters", vbox_shading);
};
