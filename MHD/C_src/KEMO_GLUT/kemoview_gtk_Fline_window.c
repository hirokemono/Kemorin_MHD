/*
 *  kemoview_gtk_Fline_window.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_Fline_window.h"


static void close_fline_CB(GtkButton *button, gpointer user_data){
    GtkWidget *window_main = GTK_WIDGET(user_data);
    struct fieldline_gtk_menu *fline_menu = (struct fieldline_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "flinemenu");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");
    
	kemoview_close_fieldline_view(kemo_sgl);
	
    init_fline_window(kemo_sgl, fline_menu, window_main);
    
	gtk_widget_queue_draw(window_main);
    draw_full(kemo_sgl);
};

static void set_fieldline_menu_box(struct kemoviewer_type *kemo_sgl,
                                   struct fieldline_gtk_menu *fline_menu,
                                   GtkWidget *window){
    fline_menu->closeButton = gtk_button_new_with_label("Close Fieldline");

    g_object_set_data(G_OBJECT(window), "flinemenu", (gpointer) fline_menu);
    g_object_set_data(G_OBJECT(window), "kemoview", (gpointer) kemo_sgl);
    g_signal_connect(G_OBJECT(fline_menu->closeButton), "clicked",
                     G_CALLBACK(close_fline_CB), (gpointer) window);
	    
	init_fieldline_menu_hbox(kemo_sgl, fline_menu);
    set_gtk_fieldline_menu(kemo_sgl, fline_menu);
	return;
}

void init_fline_window(struct kemoviewer_type *kemo_sgl,
                       struct fieldline_gtk_menu *fline_menu,
                       GtkWidget *window){
    if(fline_menu->iflag_flineBox > 0){gtk_widget_destroy(fline_menu->flineWin);};
    fline_menu->iflag_flineBox = kemoview_get_fline_parameters(kemo_sgl, DRAW_SWITCH);
    if(fline_menu->iflag_flineBox == 0){return;};

    fline_menu->flineWin = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(fline_menu->flineWin), "Fieldline");
    gtk_widget_set_size_request(fline_menu->flineWin, 150, -1);
    gtk_container_set_border_width(GTK_CONTAINER(fline_menu->flineWin), 5);

    set_fieldline_menu_box(kemo_sgl, fline_menu, window);
    GtkWidget *flineFrame = pack_fieldline_menu_frame(fline_menu);
    
    gtk_container_add(GTK_CONTAINER(fline_menu->flineWin), flineFrame);
    gtk_widget_show_all(fline_menu->flineWin);
    return;
}
