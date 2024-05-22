/*
 *  kemoview_gtk_viewmode_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_viewmode_menu.h"

static void set_viewtype_CB(GtkComboBox *combobox_viewtype, gpointer user_data)
{
    struct view_widgets *view_menu = (struct view_widgets *) user_data;
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(combobox_viewtype), "kemoview");

	int index_mode = gtk_selected_combobox_index(combobox_viewtype);
	
    set_GLFW_viewtype_mode(index_mode);
    kemoview_set_viewtype(index_mode, kemo_sgl);
    draw_full(kemo_sgl);
    
    if(kemoview_get_view_type_flag(kemo_sgl) == VIEW_STEREO){
        gtk_widget_set_sensitive(view_menu->Frame_stereo, TRUE);
    }else{
        gtk_widget_set_sensitive(view_menu->Frame_stereo, FALSE);
    };
	return;
};

GtkWidget * make_gtk_viewmode_menu_box(struct view_widgets *view_menu,
                                       struct kemoviewer_type *kemo_sgl){
	GtkWidget *hbox_viewtype;
    
    GtkWidget *label_tree_viewtype = create_fixed_label_w_index_tree();
	GtkTreeModel *model_viewtype = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_viewtype));  
    GtkTreeModel *child_model_viewtype
            = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_viewtype));
	int index = 0;
	index = append_ci_item_to_tree(index, "3D-View", VIEW_3D, child_model_viewtype);
	index = append_ci_item_to_tree(index, "Stereo-View", VIEW_STEREO, child_model_viewtype);
	index = append_ci_item_to_tree(index, "Map-View", VIEW_MAP, child_model_viewtype);
	index = append_ci_item_to_tree(index, "XY-View", VIEW_XY, child_model_viewtype);
	index = append_ci_item_to_tree(index, "XZ-View", VIEW_XZ, child_model_viewtype);
	index = append_ci_item_to_tree(index, "YZ-View", VIEW_YZ, child_model_viewtype);
	
	GtkWidget *combobox_viewtype = gtk_combo_box_new_with_model(child_model_viewtype);
	GtkCellRenderer *renderer_viewtype = gtk_cell_renderer_text_new();
	int iflag_mode = kemoview_get_view_type_flag(kemo_sgl);
	if(iflag_mode == VIEW_YZ){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_viewtype), 5);
	}else if(iflag_mode == VIEW_XZ){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_viewtype), 4);
	}else if(iflag_mode == VIEW_XY){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_viewtype), 3);
	}else if(iflag_mode == VIEW_MAP){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_viewtype), 2);
	}else if(iflag_mode == VIEW_STEREO){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_viewtype), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_viewtype), 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_viewtype), renderer_viewtype, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_viewtype), renderer_viewtype,
				"text", COLUMN_FIELD_NAME, NULL);
    g_object_set_data(G_OBJECT(combobox_viewtype), "kemoview",  (gpointer) kemo_sgl);
	g_signal_connect(G_OBJECT(combobox_viewtype), "changed",
                     G_CALLBACK(set_viewtype_CB), (gpointer) view_menu);
	
	hbox_viewtype = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_viewtype), gtk_label_new("View type: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_viewtype), combobox_viewtype, FALSE, FALSE, 0);
    return hbox_viewtype;
}

