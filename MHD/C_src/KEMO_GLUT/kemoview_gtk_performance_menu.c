/*
 *  kemoview_gtk_performance_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_performance_menu.h"

const int num_rotation =   2;
const int iaxis_rotation = 3;
const int increment_deg =  2;

static void NumThreadsChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    int ivalue = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
    kemoview_set_number_of_threads(ivalue, kemo_sgl);
    draw_full(kemo_sgl);
    return;
}

static void rot_FPS_view_CB(GtkButton *button, gpointer user_data){
    GtkEntry *FPSentry = GTK_ENTRY(user_data);
    GtkWidget *window
            = (struct rotation_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "parent");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");

    gtk_window_set_focus(GTK_WINDOW(window), NULL);
    double AverageFPS = draw_rotate_views(kemo_sgl,
                                          iaxis_rotation, increment_deg,
                                          num_rotation);
    gchar text_AverageFPS[25];
    sprintf(text_AverageFPS, "%f", (float) AverageFPS);
    gtk_entry_set_text(GTK_ENTRY(FPSentry), text_AverageFPS);
    return;
};


GtkWidget * init_num_threads_menu_frame(struct kemoviewer_type *kemo_sgl){
    int current_int = kemoview_get_number_of_threads(kemo_sgl);
    GtkAdjustment *adj_t = gtk_adjustment_new(current_int, 1, 256, 1, 1, 0.0);
    GtkWidget *spin_threads = gtk_spin_button_new(GTK_ADJUSTMENT(adj_t),0,2);
    g_signal_connect(spin_threads, "value-changed",
                     G_CALLBACK(NumThreadsChange_CB), (gpointer) kemo_sgl);
    
    GtkWidget *nthread_hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(nthread_hbox), gtk_label_new("# of threads:  "), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(nthread_hbox), spin_threads, FALSE, FALSE, 0);
    
    return wrap_into_frame_gtk("Thread parameter", nthread_hbox);
}

GtkWidget * init_FPS_test_menu_frame(struct kemoviewer_type *kemo_sgl,
                                     GtkWidget *window){
    GtkWidget * fpsTextBox = gtk_entry_new();
    gtk_entry_set_width_chars(GTK_ENTRY(fpsTextBox), 12);
    gtk_entry_set_text(GTK_ENTRY(fpsTextBox), "0.0");
    g_object_set_data(G_OBJECT(fpsTextBox), "parent", (gpointer) window);
    g_object_set_data(G_OBJECT(fpsTextBox), "kemoview", (gpointer) kemo_sgl);

    GtkWidget * fpsButton = gtk_button_new_with_label("Start FPS test");
    g_object_set_data(G_OBJECT(window), "kemoview", (gpointer) kemo_sgl);
    g_signal_connect(G_OBJECT(fpsButton), "clicked",
                     G_CALLBACK(rot_FPS_view_CB), (gpointer)fpsTextBox);

    GtkWidget * FPStest_hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(FPStest_hbox), fpsButton,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(FPStest_hbox), gtk_label_new("Average FPS:"), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(FPStest_hbox), fpsTextBox, FALSE, FALSE, 0);
    
    return wrap_into_frame_gtk("FPS test", FPStest_hbox);
}
