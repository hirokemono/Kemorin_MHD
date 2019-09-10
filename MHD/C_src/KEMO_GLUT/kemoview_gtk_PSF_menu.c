/*
 *  kemoview_gtk_PSF_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_PSF_menu.h"

static void psf_component_select_CB(GtkComboBox *combobox_comp, gpointer user_data)
{
	GtkEntry *entry = GTK_ENTRY(user_data);
	GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct colormap_view *color_vws = (struct colormap_view *) g_object_get_data(G_OBJECT(user_data), "colorview");
    
    int index_mode = gtk_selected_combobox_index(combobox_comp);
	
	kemoview_set_PSF_component(index_mode);
	
	gtk_widget_queue_draw(window);
	draw_mesh_glfw();
	return;
};

static void save_colormap_file_panel_CB(GtkButton *saveButton, gpointer user_data){
	GtkEntry *entry = GTK_ENTRY(user_data);
	GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct colormap_view *color_vws = (struct colormap_view *) g_object_get_data(G_OBJECT(user_data), "colorview");
	struct kv_string *filename = kemoview_save_file_panel(window);
	
	if(filename->string[0] != '\0'){
		write_colormap_control_file_s(filename->string, color_vws->cmap_param);
	};
	kemoview_free_kvstring(filename);
	return;
};

static void load_colormap_file_panel_CB(GtkButton *loadButton, gpointer user_data){
	GtkEntry *entry = GTK_ENTRY(user_data);
	GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct colormap_view *color_vws = (struct colormap_view *) g_object_get_data(G_OBJECT(user_data), "colorview");
	struct kv_string *filename = kemoview_read_file_panel(window);
	
	if(filename->string[0] != '\0'){
		read_colormap_control_file_s(filename->string, color_vws->cmap_param);
	};
	kemoview_free_kvstring(filename);
	
	gtk_widget_queue_draw(window);
	draw_mesh_glfw();
	return;
};



void add_psf_draw_component_box(struct kemoviewer_type *kemoviewer_data, struct colormap_view *color_vws,
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
	int ic_psf = kemoview_get_PSF_component_id();
	int ncomp = kemoview_get_PSF_num_component(if_psf);
	int icomp;
	
	GtkWidget *entry;
	entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer) window_cmap);
	g_object_set_data(G_OBJECT(entry), "colorview", (gpointer) color_vws);
	
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
					G_CALLBACK(psf_component_select_CB), (gpointer) entry);
		
		hbox_comp = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
		gtk_box_pack_start(GTK_BOX(hbox_comp), gtk_label_new("Component: "), FALSE, FALSE, 0);
		gtk_box_pack_start(GTK_BOX(hbox_comp), combobox_comp, FALSE, FALSE, 0);
		gtk_box_pack_start(GTK_BOX(box), hbox_comp, TRUE, TRUE, 0);
	};
	return;
}

void add_gtk_psf_colormap_menu(struct colormap_view *color_vws,
			GtkWidget *window, GtkWidget *box){
	GtkWidget *saveButton, *loadButton;
	
	GtkWidget *entry;
	GtkWidget *expander_cmap, *scroll_cmap, *Frame_cmap;
	GtkWidget *hbox_cmap, *vbox_cmap;
	
	entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer) window);
	g_object_set_data(G_OBJECT(entry), "colorview", (gpointer) color_vws);
	saveButton = gtk_button_new_with_label("Save colormap...");
	g_signal_connect(G_OBJECT(saveButton), "clicked", 
				G_CALLBACK(save_colormap_file_panel_CB), G_OBJECT(entry));
	
	loadButton = gtk_button_new_with_label("Load colormap...");
	g_signal_connect(G_OBJECT(loadButton), "clicked", 
				G_CALLBACK(load_colormap_file_panel_CB), G_OBJECT(entry));
	
	vbox_cmap = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	add_colormp_list_box(color_vws, vbox_cmap);
	gtk_box_pack_start(GTK_BOX(vbox_cmap), saveButton, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_cmap), loadButton, FALSE, FALSE, 0);
	
	Frame_cmap = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_cmap), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame_cmap), vbox_cmap);
	
	hbox_cmap = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_cmap), gtk_label_new("  "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_cmap), Frame_cmap, TRUE, TRUE, 0);
	
	scroll_cmap = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll_cmap),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scroll_cmap, 400, 500);
	gtk_container_add(GTK_CONTAINER(scroll_cmap), hbox_cmap);
	
	expander_cmap = gtk_expander_new_with_mnemonic("Color map editor");
	gtk_container_add(GTK_CONTAINER(expander_cmap), scroll_cmap);
	
	gtk_box_pack_start(GTK_BOX(box), expander_cmap, TRUE, FALSE, 0);
	return;
}

void make_psf_menu_box(struct kemoviewer_type *kemoviewer_data, struct colormap_view *color_vws,
			GtkWidget *window, GtkWidget *box_out){
	int i_current = kemoviewer_data->kemo_psf->psf_a->id_current;
	init_colormap_views_4_viewer(kemoviewer_data->kemo_psf->psf_m[i_current], color_vws);
	
	int if_psf = kemoview_get_PSF_field_id();
	int ic_psf = kemoview_get_PSF_component_id();
	int ncomp = kemoview_get_PSF_num_component(if_psf);
	
	add_psf_draw_component_box(kemoviewer_data, color_vws, window, box_out);
	add_gtk_isoline_menu(color_vws, window, box_out);
	add_gtk_psf_surface_menu(color_vws, window, box_out);
	add_gtk_psf_colormap_menu(color_vws, window, box_out);
	
	color_vws->psfVectorBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	if(ncomp == 3) make_gtk_psf_vector_menu(color_vws);
	gtk_box_pack_start(GTK_BOX(box_out), color_vws->psfVectorBox, FALSE, TRUE, 0);
	
	gtk_widget_show_all(box_out);
	if(ncomp == 3){
		gtk_widget_show(color_vws->psfVectorBox);
	} else {
		gtk_widget_hide(color_vws->psfVectorBox);
	};
	return;
}

