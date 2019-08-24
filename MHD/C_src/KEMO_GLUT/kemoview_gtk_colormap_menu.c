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

static void cb_close_window(GtkButton *button, gpointer user_data){
    GtkWidget *window = (GtkWidget *) user_data;
    gtk_widget_destroy(window);
};

void gtk_psf_colormap_menu(struct kv_string *title, 
			struct kemoviewer_type *kemoviewer_data){
    struct colormap_view *color_vws;
	GtkWidget *box;
	
	window_cmap = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window_cmap), title->string);
	
	g_signal_connect(window_cmap, "destroy", G_CALLBACK(gtk_main_quit), NULL);
	gtk_container_set_border_width(GTK_CONTAINER(window_cmap), 5);
	
	box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_container_add(GTK_CONTAINER(window_cmap), box);
	printf("malloc 1\n");
	color_vws = (struct colormap_view *) malloc(sizeof(struct colormap_view));
	printf("init_colormap_views_4_viewer\n");
	init_colormap_views_4_viewer(kemoviewer_data->psf_current_menu, color_vws);
	
	printf("add_colormp_list_box\n");
	add_colormp_list_box(color_vws, box);
	printf("button\n");
	GtkButton *button;
	button = gtk_button_new_from_stock(GTK_STOCK_CLOSE);
    gtk_box_pack_start(GTK_BOX(box), button, FALSE, FALSE, 0);
	printf("g_signal_connect\n");
    g_signal_connect(G_OBJECT(button), "clicked", 
                     G_CALLBACK(cb_close_window), window_cmap);
	printf("gtk_widget_show_all\n");
	
	gtk_widget_show_all(window_cmap);
	gtk_main();
	printf("dealloc_colormap_views_4_viewer\n");
	dealloc_colormap_views_4_viewer(color_vws);
	printf("free\n");
	free(color_vws);
	return;
}

