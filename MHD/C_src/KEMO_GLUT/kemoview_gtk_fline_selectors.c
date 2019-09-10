/*
 *  kemoview_gtk_fline_selectors.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_fline_selectors.h"

static void fline_field_select_CB(GtkComboBox *combobox_field, gpointer user_data)
{
    int index_mode = gtk_selected_combobox_index(combobox_field);
	
	kemoview_set_fline_color_field(index_mode);
	
	draw_mesh_glfw();
	return;
};

static void fline_component_select_CB(GtkComboBox *combobox_comp, gpointer user_data)
{
    int index_mode = gtk_selected_combobox_index(combobox_comp);
	kemoview_set_fline_color_component(index_mode);
	
	draw_mesh_glfw();
	return;
};


void add_fline_draw_field_box(GtkWidget *box){
	GtkWidget *hbox_field;
	
	GtkWidget *combobox_field;
	GtkWidget *label_tree_field;
	GtkCellRenderer *renderer_field;
	GtkTreeModel *model_field;
	GtkTreeModel *child_model_field;
	
	int index = 0;
	
    struct kv_string *colorname = kemoview_alloc_kvstring();
	int num_field = kemoview_get_fline_color_num_field();
	int if_fline = kemoview_get_fline_color_field();
	int ifld;
	
	label_tree_field = create_fixed_label_w_index_tree();
	model_field = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_field));  
	child_model_field = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_field));
	index = 0;
	for(ifld=0;ifld<num_field;ifld++){
		kemoview_get_fline_color_data_name(colorname, ifld);
		index = append_ci_item_to_tree(index, colorname->string, ifld, child_model_field);
	};
	
	combobox_field = gtk_combo_box_new_with_model(child_model_field);
	renderer_field = gtk_cell_renderer_text_new();
	gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_field), if_fline);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_field), renderer_field, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_field), renderer_field,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_field), "changed", 
				G_CALLBACK(fline_field_select_CB), NULL);
	
	hbox_field = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_field), gtk_label_new("Field: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_field), combobox_field, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(box), hbox_field, TRUE, TRUE, 0);
	return;
}


void add_fline_draw_component_box(GtkWidget *box){
	GtkWidget *hbox_comp;
	
	GtkWidget *combobox_comp;
	GtkWidget *label_tree_comp;
	GtkCellRenderer *renderer_comp;
	GtkTreeModel *model_comp;
	GtkTreeModel *child_model_comp;
	
	int index = 0;
	
	char comp_name[128];
	int icomp;
	int if_fline = kemoview_get_fline_color_field();
	int ic_fline = kemoview_get_fline_color_component();
	int ncomp =  kemoview_get_fline_color_num_comps(if_fline);
	
	if(ncomp > 1){
		label_tree_comp = create_fixed_label_w_index_tree();
		model_comp = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_comp));  
		child_model_comp = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_comp));
		index = 0;
		for(icomp=0;icomp<ncomp;icomp++){
			set_PSF_component_name(ncomp, icomp, comp_name);
			index = append_ci_item_to_tree(index, comp_name, icomp, child_model_comp);
		};
		
		combobox_comp = gtk_combo_box_new_with_model(child_model_comp);
		renderer_comp = gtk_cell_renderer_text_new();
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_comp), ic_fline);
		gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_comp), renderer_comp, TRUE);
		gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_comp), renderer_comp,
					"text", COLUMN_FIELD_NAME, NULL);
		g_signal_connect(G_OBJECT(combobox_comp), "changed", 
					G_CALLBACK(fline_component_select_CB), NULL);
		
		hbox_comp = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
		gtk_box_pack_start(GTK_BOX(hbox_comp), gtk_label_new("Component: "), FALSE, FALSE, 0);
		gtk_box_pack_start(GTK_BOX(hbox_comp), combobox_comp, FALSE, FALSE, 0);
		gtk_box_pack_start(GTK_BOX(box), hbox_comp, TRUE, TRUE, 0);
	};
	return;
}

