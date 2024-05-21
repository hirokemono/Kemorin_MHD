/*
 *  kemoview_gtk_tube_pref_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#include "kemoview_gtk_tube_pref_menu.h"

static void nTubeCornerChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    int ivalue = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
    kemoview_set_view_integer(NUM_TUBE_CORNERS_FLAG, ivalue, kemo_sgl);
    draw_full(kemo_sgl);
    return;
}

static void coasttube_thickness_CB(GtkWidget *entry, gpointer data)
{
    double current_thick;
    int current_digit;
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    
    double thick_in = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
    if(thick_in < 0) return;
    
    kemoview_get_coastline_thickness_w_exp(kemo_sgl, &current_thick, &current_digit);
    kemoview_set_coastline_thickness_w_exp(thick_in, current_digit, kemo_sgl);

    draw_full(kemo_sgl);
}

static void coasttube_digit_CB(GtkWidget *entry, gpointer data)
{
    double current_thick;
    int current_digit;
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    
    int in_digit = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
    kemoview_get_coastline_thickness_w_exp(kemo_sgl, &current_thick, &current_digit);
    kemoview_set_coastline_thickness_w_exp(current_thick, in_digit, kemo_sgl);
    
    draw_full(kemo_sgl);
}


GtkWidget * init_tube_pref_frame(struct kemoviewer_type *kemo_sgl){
    int ncorner = kemoview_get_view_integer(kemo_sgl, NUM_TUBE_CORNERS_FLAG);
    GtkAdjustment *adj_c = gtk_adjustment_new(ncorner, 1, 24, 1, 1, 0.0);
    GtkWidget *spin_nTubeCorner = gtk_spin_button_new(GTK_ADJUSTMENT(adj_c),0,2);
    g_signal_connect(spin_nTubeCorner, "value-changed",
                     G_CALLBACK(nTubeCornerChange_CB), (gpointer) kemo_sgl);

    double current_thick;
    int    current_digit;
    kemoview_get_coastline_thickness_w_exp(kemo_sgl, &current_thick, &current_digit);
    GtkWidget *adj_thick = gtk_adjustment_new(current_thick, 0, 9, 1, 1, 0.0);
    GtkWidget *adj_digit = gtk_adjustment_new(current_digit, -30, 30, 1, 1, 0.0);
    GtkWidget *spin_cline_thick = gtk_spin_button_new(GTK_ADJUSTMENT(adj_thick), 0, 0);
    GtkWidget *spin_cline_digit = gtk_spin_button_new(GTK_ADJUSTMENT(adj_digit), 0, 0);
    g_signal_connect(spin_cline_thick, "value-changed",
                     G_CALLBACK(coasttube_thickness_CB), (gpointer) kemo_sgl);
    g_signal_connect(spin_cline_digit, "value-changed",
                     G_CALLBACK(coasttube_digit_CB), (gpointer) kemo_sgl);
    

    GtkWidget *nTubeCorner_hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(nTubeCorner_hbox), gtk_label_new("# of tube corners:  "), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(nTubeCorner_hbox), spin_nTubeCorner, FALSE, FALSE, 0);

    GtkWidget *hbox_thickness = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_thickness), gtk_label_new("Thickness: "), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_thickness), spin_cline_thick, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_thickness), gtk_label_new("X 10^"), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_thickness), spin_cline_digit, TRUE, TRUE, 0);
    
    GtkWidget *vbox_coasttube = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_pack_start(GTK_BOX(vbox_coasttube), nTubeCorner_hbox, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_coasttube), hbox_thickness, TRUE, TRUE, 0);
    
    return wrap_into_frame_gtk("Tube parameters", vbox_coasttube);
};
