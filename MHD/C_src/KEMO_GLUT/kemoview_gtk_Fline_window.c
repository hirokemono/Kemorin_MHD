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
    GtkWidget *itemTEvo = GTK_WIDGET(user_data);
    struct fieldline_gtk_menu *fline_menu
            = (struct fieldline_gtk_menu *) g_object_get_data(G_OBJECT(button), "flinemenu");
    struct kemoviewer_gl_type *kemo_gl
            = (struct fieldline_gtk_menu *) g_object_get_data(G_OBJECT(button), "kemoview_gl");

	kemoview_close_fieldline_view(kemo_gl->kemoview_data);
	
    gtk_widget_destroy(fline_menu->flineWin);
    activate_evolution_menu(kemo_gl->kemoview_data, itemTEvo);
    draw_full_gl(kemo_gl);
};

static void set_fieldline_menu_box(struct kemoviewer_gl_type *kemo_gl,
                                   struct fieldline_gtk_menu *fline_menu,
                                   GtkWidget *itemTEvo){
    fline_menu->closeButton = gtk_button_new_with_label("Close Fieldline");

    g_object_set_data(G_OBJECT(fline_menu->closeButton), "kemoview_gl", (gpointer) kemo_gl);
    g_object_set_data(G_OBJECT(fline_menu->closeButton), "flinemenu", (gpointer) fline_menu);
    g_signal_connect(G_OBJECT(fline_menu->closeButton), "clicked",
                     G_CALLBACK(close_fline_CB), itemTEvo);
	    
	init_fieldline_menu_hbox(kemo_gl, fline_menu);
    set_gtk_fieldline_menu(kemo_gl, fline_menu);
	return;
}

void init_fline_window(struct kemoviewer_gl_type *kemo_gl,
                       struct fieldline_gtk_menu *fline_menu,
                       GtkWidget *main_window, GtkWidget *itemTEvo){
    fline_menu->iflag_flineBox = kemoview_get_fline_parameters(kemo_gl->kemoview_data,
                                                               DRAW_SWITCH);
    if(fline_menu->iflag_flineBox == 0){return;};

    gint win_upperleft[2], size_xy[2];
    gtk_window_get_position(GTK_WINDOW(main_window), &win_upperleft[0], &win_upperleft[1]);
    gtk_window_get_size(GTK_WINDOW(main_window), &size_xy[0], &size_xy[1]);

    fline_menu->flineWin = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_move(GTK_WINDOW(fline_menu->flineWin),
                    win_upperleft[0], (win_upperleft[1]+size_xy[1]+56));
    gtk_window_set_title(GTK_WINDOW(fline_menu->flineWin), "Fieldline");
    gtk_widget_set_size_request(fline_menu->flineWin, 150, -1);
    gtk_container_set_border_width(GTK_CONTAINER(fline_menu->flineWin), 5);

    set_fieldline_menu_box(kemo_gl, fline_menu, itemTEvo);
    GtkWidget *flineFrame = pack_fieldline_menu_frame(fline_menu);
    
    gtk_container_add(GTK_CONTAINER(fline_menu->flineWin), flineFrame);
    gtk_widget_show_all(fline_menu->flineWin);
    return;
}
