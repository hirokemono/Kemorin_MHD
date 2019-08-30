/*
 *  kemoview_gtk_colormap_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_colormap_menu.h"

GtkWidget *window_cmap;

static char viewtype_title[80] = "3D-View";

static void close_psf_CB(GtkButton *button, gpointer user_data){
	int nload_psf;
	GtkWidget *window = (GtkWidget *) user_data;
	set_viewtype_mode_glut(VIEW_3D, viewtype_title);
	nload_psf = kemoview_close_PSF_view();
};

static void close_window_CB(GtkButton *button, gpointer user_data){
    GtkWidget *window = (GtkWidget *) user_data;
    gtk_widget_destroy(window);
};

static void save_colormap_file_panel(GtkButton *saveButton, gpointer user_data){
	struct colormap_view *color_vws = (struct colormap_view *) user_data;
	struct kv_string *filename = kemoview_save_file_panel(window_cmap);
	
	if(filename->string[0] != '\0'){
		write_colormap_control_file_s(filename->string, color_vws->cmap_param);
	};
	kemoview_free_kvstring(filename);
	return;
};
static void load_colormap_file_panel(GtkButton *loadButton, gpointer user_data){
	struct colormap_view *color_vws = (struct colormap_view *) user_data;
	struct kv_string *filename = kemoview_read_file_panel(window_cmap);
	
	if(filename->string[0] != '\0'){
		read_colormap_control_file_s(filename->string, color_vws->cmap_param);
	};
	kemoview_free_kvstring(filename);
	return;
};

void add_gtk_psf_colormap_menu(struct colormap_view *color_vws, GtkWidget *box){
	GtkButton *saveButton, *loadButton;
	
	GtkWidget *expander_cmap, *scroll_cmap, *Frame_cmap;
	GtkWidget *hbox_cmap, *vbox_cmap;
	
	saveButton = gtk_button_new_with_label("Save colormap...");
	g_signal_connect(G_OBJECT(saveButton), "clicked", 
				G_CALLBACK(save_colormap_file_panel), G_OBJECT(color_vws));
	
	loadButton = gtk_button_new_with_label("Load colormap...");
	g_signal_connect(G_OBJECT(loadButton), "clicked", 
				G_CALLBACK(load_colormap_file_panel), G_OBJECT(color_vws));
	
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

void gtk_psf_colormap_menu(struct kemoviewer_type *kemoviewer_data){
	GtkWidget *box;
	GtkButton *closeButton, *updateButton;
	
	int index = 0;
	int iflag_sfcolor;
	
	
	struct colormap_view *color_vws
			= (struct colormap_view *) malloc(sizeof(struct colormap_view));
	init_colormap_views_4_viewer(kemoviewer_data->psf_current_menu, color_vws);
	
	
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
	
	int if_psf = kemoview_get_PSF_field_id();
	int ncomp = kemoview_get_PSF_num_component(if_psf);
	
	box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_container_add(GTK_CONTAINER(window_cmap), box);
	add_current_psf_set_box(kemoviewer_data, window_cmap, box);
	add_psf_draw_field_box(kemoviewer_data, window_cmap, box);
	add_psf_draw_component_box(kemoviewer_data, window_cmap, box);
	add_gtk_isoline_menu(color_vws, window_cmap, box);
	add_gtk_psf_surface_menu(color_vws, window_cmap, box);
	add_gtk_psf_colormap_menu(color_vws, box);
	if(ncomp == 3){
		add_gtk_psf_vector_menu(color_vws, box);
	};
	gtk_box_pack_start(GTK_BOX(box), closeButton, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box), updateButton, FALSE, FALSE, 0);
	
	gtk_widget_show_all(window_cmap);
	gtk_main();
	dealloc_colormap_views_4_viewer(color_vws);
	free(color_vws);
	return;
}
