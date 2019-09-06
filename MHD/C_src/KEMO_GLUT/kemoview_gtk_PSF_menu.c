/*
 *  kemoview_gtk_PSF_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_PSF_menu.h"

GtkWidget *window_cmap;

static void close_psf_CB(GtkButton *button, gpointer user_data){
	int nload_psf;
	GtkWidget *window = (GtkWidget *) user_data;
	set_viewtype_mode_glfw(VIEW_3D);
	nload_psf = kemoview_close_PSF_view();
	
	gtk_widget_queue_draw(window);
	draw_mesh_glfw();
};

static void close_window_CB(GtkButton *button, gpointer user_data){
    GtkWidget *window = (GtkWidget *) user_data;
    gtk_widget_destroy(window);
	
	gtk_widget_queue_draw(window);
	draw_mesh_glfw();
};

static void current_psf_select_CB(GtkComboBox *combobox_sfcolor, gpointer user_data)
{
	GtkWidget *window = (GtkWidget *) user_data;
    int index_mode = gtk_selected_combobox_index(combobox_sfcolor);
	
	kemoview_set_current_PSF(index_mode);
	gtk_widget_queue_draw(window);
	draw_mesh_glfw();
	return;
};

static void psf_field_select_CB(GtkComboBox *combobox_field, gpointer user_data)
{
	GtkEntry *entry = GTK_ENTRY(user_data);
	GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct colormap_view *color_vws = (struct colormap_view *) g_object_get_data(G_OBJECT(user_data), "colorview");
	struct kemoviewer_type *kemoviewer_data = (struct colormap_view *) g_object_get_data(G_OBJECT(user_data), "kemoview");
	
    int index_mode = gtk_selected_combobox_index(combobox_field);
    
	kemoview_set_PSF_field(index_mode);
	
	gtk_widget_destroy(color_vws->psfBox);
	color_vws->psfBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	make_psf_menu_box(kemoviewer_data, color_vws, window);
	
	gtk_widget_queue_draw(window);
	draw_mesh_glfw();
	return;
};

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


void add_current_psf_set_box(struct kemoviewer_type *kemoviewer_data, struct colormap_view *color_vws,
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
	
	GtkWidget *entry;
	entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer) window_cmap);
	g_object_set_data(G_OBJECT(entry), "colorview", (gpointer) color_vws);
	
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
					G_CALLBACK(current_psf_select_CB), (gpointer) entry);
		
		hbox_psfs = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
		gtk_box_pack_start(GTK_BOX(hbox_psfs), gtk_label_new("Current PSF: "), FALSE, FALSE, 0);
		gtk_box_pack_start(GTK_BOX(hbox_psfs), combobox_psfs, FALSE, FALSE, 0);
		
		gtk_box_pack_start(GTK_BOX(box), hbox_psfs, TRUE, TRUE, 0);
	}
	
	return;
}

void add_psf_draw_field_box(struct kemoviewer_type *kemoviewer_data, struct colormap_view *color_vws, 
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
	int ic_psf = kemoview_get_PSF_component_id();
	int ifld;
	
	GtkWidget *entry;
	entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer) window_cmap);
	g_object_set_data(G_OBJECT(entry), "colorview", (gpointer) color_vws);
	g_object_set_data(G_OBJECT(entry), "kemoview", (gpointer) kemoviewer_data);
	
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
				G_CALLBACK(psf_field_select_CB), (gpointer) entry);
	
	hbox_field = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_field), gtk_label_new("Field: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_field), combobox_field, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(box), hbox_field, TRUE, TRUE, 0);
	return;
}


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
			GtkWidget *window){
	init_colormap_views_4_viewer(kemoviewer_data->psf_current_menu, color_vws);
	
	int if_psf = kemoview_get_PSF_field_id();
	int ic_psf = kemoview_get_PSF_component_id();
	int ncomp = kemoview_get_PSF_num_component(if_psf);
	
	add_current_psf_set_box(kemoviewer_data, color_vws, window, color_vws->psfBox);
	add_psf_draw_field_box(kemoviewer_data, color_vws, window, color_vws->psfBox);
	add_psf_draw_component_box(kemoviewer_data, color_vws, window, color_vws->psfBox);
	add_gtk_isoline_menu(color_vws, window, color_vws->psfBox);
	add_gtk_psf_surface_menu(color_vws, window, color_vws->psfBox);
	add_gtk_psf_colormap_menu(color_vws, window, color_vws->psfBox);
	
	color_vws->psfVectorBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	if(ncomp == 3) make_gtk_psf_vector_menu(color_vws);
	gtk_box_pack_start(GTK_BOX(color_vws->psfBox), color_vws->psfVectorBox, FALSE, TRUE, 0);
	dealloc_colormap_views_4_viewer(color_vws);
	
	gtk_widget_show_all(color_vws->psfBox);
	if(ncomp == 3){
		gtk_widget_show(color_vws->psfVectorBox);
	} else {
		gtk_widget_hide(color_vws->psfVectorBox);
	};
	return;
}

void gtk_psf_menu_box(struct kemoviewer_type *kemoviewer_data, struct colormap_view *color_vws,
			GtkWidget *window, GtkWidget *box_out){
	
	color_vws->psfBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	make_psf_menu_box(kemoviewer_data, color_vws, window);
	wrap_into_frame_gtk("Surfaces", color_vws->psfBox, box_out);
	gtk_widget_show(box_out);
	return;
}


void gtk_psf_menu_window(struct kemoviewer_type *kemoviewer_data){
	struct colormap_view *color_vws = (struct colormap_view *) malloc(sizeof(struct colormap_view));
	GtkWidget *box;
	GtkWidget *closeButton, *updateButton;
	
	window_cmap = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window_cmap), "PSF data");
	
	g_signal_connect(window_cmap, "destroy", G_CALLBACK(gtk_main_quit), NULL);
	gtk_container_set_border_width(GTK_CONTAINER(window_cmap), 5);
	
	closeButton = gtk_button_new_with_label("Close Current PSF");
	g_signal_connect(G_OBJECT(closeButton), "clicked", 
				G_CALLBACK(close_psf_CB), window_cmap);
	
	updateButton = gtk_button_new_with_label("Update");
	g_signal_connect(G_OBJECT(updateButton), "clicked", 
				G_CALLBACK(close_window_CB), window_cmap);
	
	box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_psf_menu_box(kemoviewer_data, color_vws, window_cmap, box);
	
	gtk_box_pack_start(GTK_BOX(box), closeButton, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box), updateButton, FALSE, FALSE, 0);
	
	gtk_widget_show_all(window_cmap);
	gtk_main();
	free(color_vws);
	return;
}
