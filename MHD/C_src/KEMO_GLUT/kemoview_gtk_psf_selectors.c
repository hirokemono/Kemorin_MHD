/*
 *  kemoview_gtk_psf_selectors.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_psf_selectors.h"

static void current_psf_select_CB(GtkComboBox *combobox_sfcolor, gpointer user_data)
{
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_sfcolor);
    GtkTreeIter iter;
    cairo_t *cr;
    
    gchar *row_string;
    int index_field;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_sfcolor);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_MATH, &index_mode, -1);
    
    printf("Selected mode %d, %s\n", index_mode, row_string);
	kemoview_set_current_PSF(index_mode);
	
//	draw_mesh_w_menu();
	return;
};

static void psf_field_select_CB(GtkComboBox *combobox_field, gpointer user_data)
{
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_field);
    GtkTreeIter iter;
    cairo_t *cr;
    
    gchar *row_string;
    int index_field;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_field);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_MATH, &index_mode, -1);
    
    printf("Selected mode %d, %s\n", index_mode, row_string);
	kemoview_set_PSF_field(index_mode);
	
//	draw_mesh_w_menu();
	return;
};

static void psf_component_select_CB(GtkComboBox *combobox_comp, gpointer user_data)
{
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_comp);
    GtkTreeIter iter;
    cairo_t *cr;
    
    gchar *row_string;
    int index_field;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_comp);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_MATH, &index_mode, -1);
    
    printf("Selected mode %d, %s\n", index_mode, row_string);
	kemoview_set_PSF_component(index_mode);
	
//	draw_mesh_w_menu();
	return;
};


void add_current_psf_set_box(struct kemoviewer_type *kemoviewer_data,
			GtkWidget *window_cmap, GtkWidget *box){
	GtkWidget *hbox_psfs;
	
	GtkWidget *combobox_psfs;
	GtkWidget *label_tree_psfs;
	GtkCellRenderer *renderer_psfs;
	GtkTreeModel *model_psfs;
	GtkTreeModel *child_model_psfs;
	
	int index = 0;
	int ipsf, istep;
	
	struct kv_string *stripped_filehead;
	char label_tmp[512];
	int id_current = kemoview_get_curent_PSF_ID();
	int index_current;
	int num_psf = 0;
	for (ipsf=0; ipsf< kemoview_get_PSF_max_loaded(); ipsf++){
		if(kemoview_get_PSF_loaded_flag(ipsf) > 0) {num_psf = num_psf + 1;};
	};
	
	if(num_psf > 1){
		label_tree_psfs = create_fixed_label_w_index_tree();
		model_psfs = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_psfs));  
		child_model_psfs = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_psfs));
		index = 0;
		for (ipsf=0; ipsf< kemoview_get_PSF_max_loaded(); ipsf++){
			if(ipsf == id_current) {index_current = index;};
			if(kemoview_get_PSF_loaded_flag(ipsf) > 0) {
				kemoview_set_current_PSF(ipsf);
				stripped_filehead = kemoview_alloc_kvstring();
				istep = kemoview_get_PSF_file_prefix(stripped_filehead);
				sprintf(label_tmp, "%d: %s", ipsf, stripped_filehead->string);
				index = append_ci_item_to_tree(index, label_tmp,
											   ipsf, child_model_psfs);
				kemoview_free_kvstring(stripped_filehead);
			};
			kemoview_set_current_PSF(id_current);
		};
		
		combobox_psfs = gtk_combo_box_new_with_model(child_model_psfs);
		renderer_psfs = gtk_cell_renderer_text_new();
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_psfs), index_current);
		gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_psfs), renderer_psfs, TRUE);
		gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_psfs), renderer_psfs,
					"text", COLUMN_FIELD_NAME, NULL);
		g_signal_connect(G_OBJECT(combobox_psfs), "changed", 
					G_CALLBACK(current_psf_select_CB), (gpointer) window_cmap);
		
		hbox_psfs = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
		gtk_box_pack_start(GTK_BOX(hbox_psfs), gtk_label_new("Current PSF: "), FALSE, FALSE, 0);
		gtk_box_pack_start(GTK_BOX(hbox_psfs), combobox_psfs, FALSE, FALSE, 0);
		
		gtk_box_pack_start(GTK_BOX(box), hbox_psfs, TRUE, TRUE, 0);
	}
	
	return;
}

void add_psf_draw_field_box(struct kemoviewer_type *kemoviewer_data,
			GtkWidget *window_cmap, GtkWidget *box){
	GtkWidget *hbox_field;
	
	GtkWidget *combobox_field;
	GtkWidget *label_tree_field;
	GtkCellRenderer *renderer_field;
	GtkTreeModel *model_field;
	GtkTreeModel *child_model_field;
	
	int index = 0;
	
    struct kv_string *colorname = kemoview_alloc_kvstring();
	int num_field = kemoview_get_PSF_num_field();
	int if_psf = kemoview_get_PSF_field_id();
	int ifld;
	
	label_tree_field = create_fixed_label_w_index_tree();
	model_field = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_field));  
	child_model_field = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_field));
	index = 0;
	for(ifld=0;ifld<num_field;ifld++){
		kemoview_get_PSF_field_name(colorname, ifld);
		index = append_ci_item_to_tree(index, colorname->string, ifld, child_model_field);
	};
	
	combobox_field = gtk_combo_box_new_with_model(child_model_field);
	renderer_field = gtk_cell_renderer_text_new();
	gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_field), if_psf);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_field), renderer_field, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_field), renderer_field,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_field), "changed", 
				G_CALLBACK(psf_field_select_CB), (gpointer) window_cmap);
	
	hbox_field = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_field), gtk_label_new("Field: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_field), combobox_field, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(box), hbox_field, TRUE, TRUE, 0);
	return;
}


void add_psf_draw_component_box(struct kemoviewer_type *kemoviewer_data,
			GtkWidget *window_cmap, GtkWidget *box){
	GtkWidget *hbox_comp;
	
	GtkWidget *combobox_comp;
	GtkWidget *label_tree_comp;
	GtkCellRenderer *renderer_comp;
	GtkTreeModel *model_comp;
	GtkTreeModel *child_model_comp;
	
	int index = 0;
	
	char comp_name[1024];
	int if_psf = kemoview_get_PSF_field_id();
	int ncomp = kemoview_get_PSF_num_component(if_psf);
	int icomp;
	
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
		icomp = kemoview_get_PSF_component_id();
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_comp), icomp);
		gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_comp), renderer_comp, TRUE);
		gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_comp), renderer_comp,
					"text", COLUMN_FIELD_NAME, NULL);
		g_signal_connect(G_OBJECT(combobox_comp), "changed", 
					G_CALLBACK(psf_component_select_CB), (gpointer) window_cmap);
		
		hbox_comp = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
		gtk_box_pack_start(GTK_BOX(hbox_comp), gtk_label_new("Component: "), FALSE, FALSE, 0);
		gtk_box_pack_start(GTK_BOX(hbox_comp), combobox_comp, FALSE, FALSE, 0);
		gtk_box_pack_start(GTK_BOX(box), hbox_comp, TRUE, TRUE, 0);
	};
	return;
}

