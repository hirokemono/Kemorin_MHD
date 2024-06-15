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
    struct fieldline_gtk_menu *fline_gmenu
            = (struct fieldline_gtk_menu *) g_object_get_data(G_OBJECT(button), "flinemenu");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(button), "kemoview_gl");

	kemoview_close_fieldline_view(kemo_gl->kemoview_data);
	
    gtk_widget_destroy(fline_gmenu->flineWin);
    activate_evolution_menu(kemo_gl->kemoview_data, itemTEvo);
    draw_full_gl(kemo_gl);
};

static void set_fieldline_menu_box(struct kemoviewer_gl_type *kemo_gl,
                                   struct fieldline_gtk_menu *fline_gmenu,
                                   GtkWidget *itemTEvo){
    fline_gmenu->closeButton = gtk_button_new_with_label("Close Fieldline");

    g_object_set_data(G_OBJECT(fline_gmenu->closeButton), "kemoview_gl", (gpointer) kemo_gl);
    g_object_set_data(G_OBJECT(fline_gmenu->closeButton), "flinemenu", (gpointer) fline_gmenu);
    g_signal_connect(G_OBJECT(fline_gmenu->closeButton), "clicked",
                     G_CALLBACK(close_fline_CB), itemTEvo);
	    
	init_fieldline_menu_hbox(kemo_gl, fline_gmenu);
    
    int itype_fline = kemoview_get_line_type_flag(kemo_gl->kemoview_data);
    if(itype_fline == 0){
        gtk_switch_set_active(GTK_SWITCH(fline_gmenu->switch_tube), FALSE);
    } else {
        gtk_switch_set_active(GTK_SWITCH(fline_gmenu->switch_tube), TRUE);
    };
    
    set_gtk_fieldline_menu(kemo_gl, fline_gmenu);
	return;
}

static void set_tracer_menu_box(struct kemoviewer_gl_type *kemo_gl,
                                struct fieldline_gtk_menu *fline_gmenu,
                                GtkWidget *itemTEvo){
    fline_gmenu->closeButton = gtk_button_new_with_label("Close Fieldline");

    g_object_set_data(G_OBJECT(fline_gmenu->closeButton), "kemoview_gl", (gpointer) kemo_gl);
    g_object_set_data(G_OBJECT(fline_gmenu->closeButton), "flinemenu", (gpointer) fline_gmenu);
    g_signal_connect(G_OBJECT(fline_gmenu->closeButton), "clicked",
                     G_CALLBACK(close_fline_CB), itemTEvo);
        
    init_tracer_menu_hbox(kemo_gl, fline_gmenu);
    set_gtk_fieldline_menu(kemo_gl, fline_gmenu);
    return;
}


void init_fline_window(struct kemoviewer_gl_type *kemo_gl,
                       struct fieldline_gtk_menu *fline_gmenu,
                       GtkWidget *main_window, GtkWidget *itemTEvo){
    fline_gmenu->iflag_flineBox = kemoview_get_VIZ_draw_flags(kemo_gl->kemoview_data,
                                                             FIELDLINE_RENDERING);
    if(fline_gmenu->iflag_flineBox == 0){return;};

    gint win_upperleft[2], size_xy[2];
    gtk_window_get_position(GTK_WINDOW(main_window), &win_upperleft[0], &win_upperleft[1]);
    gtk_window_get_size(GTK_WINDOW(main_window), &size_xy[0], &size_xy[1]);

    fline_gmenu->flineWin = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_move(GTK_WINDOW(fline_gmenu->flineWin),
                    win_upperleft[0], (win_upperleft[1]+size_xy[1]+56));
    gtk_window_set_title(GTK_WINDOW(fline_gmenu->flineWin), "Fieldline");
    gtk_widget_set_size_request(fline_gmenu->flineWin, 150, -1);
    gtk_container_set_border_width(GTK_CONTAINER(fline_gmenu->flineWin), 5);

    fline_gmenu->fline_color_vws = alloc_colormap_view();
    set_fieldline_menu_box(kemo_gl, fline_gmenu, itemTEvo);
    fline_gmenu->fline_frame = pack_fieldline_menu_frame(fline_gmenu);
    
    gtk_container_add(GTK_CONTAINER(fline_gmenu->flineWin), fline_gmenu->fline_frame);
    gtk_widget_show_all(fline_gmenu->flineWin);
    return;
}


void init_tracer_window(struct kemoviewer_gl_type *kemo_gl,
                        struct fieldline_gtk_menu *tracer_gmenu,
                        GtkWidget *main_window, GtkWidget *itemTEvo){
    tracer_gmenu->iflag_flineBox = kemoview_get_VIZ_draw_flags(kemo_gl->kemoview_data,
                                                               TRACER_RENDERING);
    if(tracer_gmenu->iflag_flineBox == 0){return;};

    gint win_upperleft[2], size_xy[2];
    gtk_window_get_position(GTK_WINDOW(main_window), &win_upperleft[0], &win_upperleft[1]);
    gtk_window_get_size(GTK_WINDOW(main_window), &size_xy[0], &size_xy[1]);

    tracer_gmenu->flineWin = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_move(GTK_WINDOW(tracer_gmenu->flineWin),
                    win_upperleft[0], (win_upperleft[1]+size_xy[1]+56));
    gtk_window_set_title(GTK_WINDOW(tracer_gmenu->flineWin), "Tracer");
    gtk_widget_set_size_request(tracer_gmenu->flineWin, 150, -1);
    gtk_container_set_border_width(GTK_CONTAINER(tracer_gmenu->flineWin), 5);

    tracer_gmenu->fline_color_vws = alloc_colormap_view();
    set_tracer_menu_box(kemo_gl, tracer_gmenu, itemTEvo);
    tracer_gmenu->fline_frame = pack_tracer_menu_frame(tracer_gmenu);
    
    gtk_container_add(GTK_CONTAINER(tracer_gmenu->flineWin), tracer_gmenu->fline_frame);
    gtk_widget_show_all(tracer_gmenu->flineWin);
    return;
}
