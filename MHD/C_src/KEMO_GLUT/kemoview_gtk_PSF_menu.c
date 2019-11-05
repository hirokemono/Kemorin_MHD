/*
 *  kemoview_gtk_PSF_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_PSF_menu.h"

static void save_colormap_file_panel_CB(GtkButton *saveButton, gpointer user_data){
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
	GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct colormap_view *color_vws = (struct colormap_view *) g_object_get_data(G_OBJECT(user_data), "colorview");
	struct kv_string *filename = kemoview_read_file_panel(window);
	
	if(filename->string[0] != '\0'){
		read_colormap_control_file_s(filename->string, color_vws->cmap_param);
	};
	kemoview_free_kvstring(filename);
	
	gtk_widget_queue_draw(window);
	draw_full();
	return;
};


void add_gtk_psf_colormap_menu(struct colormap_view *color_vws,
			GtkWidget *window, GtkWidget *box){
	GtkWidget *saveButton, *loadButton;
	
	GtkWidget *entry;
	GtkWidget *vbox_cmap;
	
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
	add_kemoview_colormap_list_box(color_vws, vbox_cmap);
	gtk_box_pack_start(GTK_BOX(vbox_cmap), saveButton, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_cmap), loadButton, FALSE, FALSE, 0);
	
	wrap_into_expanded_frame_gtk("Color map editor", 420, 450, vbox_cmap, box);
	return;
}

void make_psf_menu_box(struct colormap_view *color_vws,
			GtkWidget *window, GtkWidget *box_out){
	int if_psf = kemoview_get_each_PSF_field_param(FIELD_SEL_FLAG);
	int ncomp = kemoview_get_PSF_num_component(if_psf);
	
	add_gtk_isoline_menu(window, box_out);
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

