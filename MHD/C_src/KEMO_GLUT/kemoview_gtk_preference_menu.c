/*
 *  kemoview_gtk_preference_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_preference_menu.h"

static void kemoview_gtk_BGcolorsel(GtkButton *button, gpointer data){
	float color[4];
	GtkWindow *window = GTK_WINDOW(data);
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(data), "kemoview");

	int iflag_set = kemoview_gtk_colorsel_CB(window, color);
	if(iflag_set > 0){
        kemoview_set_background_color(color, kemo_sgl);
        kemoview_gl_background_color(kemo_sgl);
    };
	
    draw_full(kemo_sgl);
	return;
}

GtkWidget * init_preference_vbox(struct kemoviewer_type *kemoviewer_data,
                                 struct lightparams_view *lightparams_vws,
                                 struct rotation_gtk_menu *rot_gmenu,
                                 GtkWidget *window){
    GtkWidget *pref_vbox;
    
    float color[4];
	kemoview_get_background_color(kemoviewer_data, color);
	
	/* Set buttons   */
    GtkWidget *BGselButton = gtk_button_new_with_label("Set Background");
    g_object_set_data(G_OBJECT(window), "kemoview", (gpointer) kemoviewer_data);
	g_signal_connect(G_OBJECT(BGselButton), "clicked",
                     G_CALLBACK(kemoview_gtk_BGcolorsel), (gpointer)window);
    
    GtkWidget *lighting_frame =  init_lighting_frame(kemoviewer_data, 
                                                     lightparams_vws);
    GtkWidget *Shading_frame =   shading_mode_menu_frame(kemoviewer_data);
    GtkWidget *Tube_frame =      init_tube_pref_frame(kemoviewer_data);
    GtkWidget *coastline_frame = init_coastline_pref_menu(kemoviewer_data);
    GtkWidget *Axis_frame =      init_axis_position_menu(kemoviewer_data);
    GtkWidget *FPS_frame =       init_FPS_test_menu_frame(kemoviewer_data, rot_gmenu, window);
    GtkWidget *NumThread_frame = init_num_threads_menu_frame(kemoviewer_data);

    
    pref_vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_pack_start(GTK_BOX(pref_vbox), BGselButton, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_vbox), NumThread_frame, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_vbox), lighting_frame, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(pref_vbox), Tube_frame, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_vbox), Shading_frame, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_vbox), coastline_frame, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_vbox), Axis_frame, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_vbox), FPS_frame, FALSE, FALSE, 0);
    return pref_vbox;
}

GtkWidget * init_preference_frame(struct kemoviewer_type *kemoviewer_data,
                                  struct lightparams_view *lightparams_vws,
                                  struct rotation_gtk_menu *rot_gmenu,
                                  GtkWidget *window){
    GtkWidget *frame_pref  = gtk_frame_new("Preferences");
    gtk_frame_set_shadow_type(GTK_FRAME(frame_pref), GTK_SHADOW_IN);
    GtkWidget *pref_vbox = init_preference_vbox(kemoviewer_data, 
                                                lightparams_vws, 
                                                rot_gmenu, window);
    gtk_container_add(GTK_CONTAINER(frame_pref), pref_vbox);
    return frame_pref;
}

GtkWidget * init_preference_expander(struct kemoviewer_type *kemoviewer_data,
                                     struct lightparams_view *lightparams_vws,
                                     struct rotation_gtk_menu *rot_gmenu,
                                     GtkWidget *window){
    GtkWidget *expander_pref;
    GtkWidget *pref_vbox = init_preference_vbox(kemoviewer_data,
                                                lightparams_vws, 
                                                rot_gmenu, window);
    expander_pref = wrap_into_scroll_expansion_gtk("Preferences", 160, 400,
                                                   window, pref_vbox);
    return expander_pref;
}
