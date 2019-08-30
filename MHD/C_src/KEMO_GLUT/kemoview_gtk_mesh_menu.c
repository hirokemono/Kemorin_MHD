/*
 *  kemoview_gtk_mesh_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_mesh_menu.h"

GtkWidget *window_mesh;

static void kemoview_BG_cancel(GtkButton *button, gpointer data){
	gtk_widget_destroy(window_mesh);
	gtk_main_quit();
	return;
};

static void kemoview_BG_close(GtkButton *button, gpointer data){
	kemoview_close_mesh_view();
	gtk_widget_destroy(window_mesh);
	gtk_main_quit();
	return;
};

void gtk_mesh_menu(struct kemoviewer_type *kemoviewer_data){
	GtkWidget *hbox, *vbox;
	
	GtkWidget *entry;
	GtkWidget *closeMeshButton, *cancelButton;
	
    struct kemoview_mesh_view *mesh_vws;
	
	
	window_mesh = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	
	gtk_window_set_title(GTK_WINDOW(window_mesh), "Mesh viewer");
	gtk_widget_set_size_request(window_mesh, 150, -1);
	gtk_container_set_border_width(GTK_CONTAINER(window_mesh), 5);
	g_signal_connect(G_OBJECT(window_mesh), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	
	/* Set buttons   */
	entry = gtk_entry_new();
	closeMeshButton = gtk_button_new_with_label("Close mesh");
	g_signal_connect(G_OBJECT(closeMeshButton), "clicked", 
				G_CALLBACK(kemoview_BG_close), (gpointer)entry);
	cancelButton = gtk_button_new_with_label("Update");
	g_signal_connect(G_OBJECT(cancelButton), "clicked", 
				G_CALLBACK(kemoview_BG_cancel), (gpointer)entry);
	
	mesh_vws = (struct kemoview_mesh_view *) malloc(sizeof(struct kemoview_mesh_view));
    init_mesh_views_4_viewer(kemoviewer_data->kemo_mesh->mesh_d, mesh_vws);
	
	vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_container_add(GTK_CONTAINER(window_mesh), vbox);
	
	gtk_box_pack_start(GTK_BOX(vbox), closeMeshButton, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), cancelButton, FALSE, FALSE, 0);
	
	
	gtk_widget_show_all(window_mesh);
	gtk_main();
	printf("Tako  adagfarg\n");
	dealloc_mesh_views_4_viewer(mesh_vws);
	return;
}
