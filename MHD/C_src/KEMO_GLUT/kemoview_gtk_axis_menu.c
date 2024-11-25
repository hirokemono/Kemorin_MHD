/*
 *  kemoview_gtk_axis_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_axis_menu.h"

static void draw_axis_switch_CB(GObject *switch_bar, GParamSpec *pspec, gpointer data){
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) data;
    int iflag = gtk_switch_get_state(GTK_SWITCH(switch_bar));
    kemoview_set_object_property_flags(AXIS_TOGGLE, iflag,
                                       kemo_gl->kemoview_data);
	
	draw_full_gl(kemo_gl);
	return;
};
static void draw_coastline_switch_CB(GObject *switch_bar, GParamSpec *pspec, gpointer data){
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) data;
    int iflag = gtk_switch_get_state(GTK_SWITCH(switch_bar));
    kemoview_set_object_property_flags(COASTLINE_SWITCH, iflag,
                                       kemo_gl->kemoview_data);
	
    draw_full_gl(kemo_gl);
	return;
};
static void draw_sph_grid_switch_CB(GObject *switch_bar, GParamSpec *pspec, gpointer data){
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) data;
    int iflag = gtk_switch_get_state(GTK_SWITCH(switch_bar));
    kemoview_set_object_property_flags(SPHEREGRID_SWITCH, iflag,
                                       kemo_gl->kemoview_data);
	
    draw_full_gl(kemo_gl);
	return;
};
static void draw_tangent_cyl_switch_CB(GObject *switch_bar, GParamSpec *pspec, gpointer data){
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) data;
    int iflag = gtk_switch_get_state(GTK_SWITCH(switch_bar));
    kemoview_set_object_property_flags(TANGENT_CYLINDER_SWITCH, iflag,
                                       kemo_gl->kemoview_data);
    
    draw_full_gl(kemo_gl);
    return;
};
static void coastline_radius_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) data;
	double radius = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_coastline_radius(radius, kemo_gl->kemoview_data);
	
    draw_full_gl(kemo_gl);
}
static void ICB_radius_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) data;
    double radius = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
    kemoview_set_inner_core_radius(radius, kemo_gl->kemoview_data);
    
    draw_full_gl(kemo_gl);
}



GtkWidget * make_axis_menu_box(struct kemoviewer_gl_type *kemo_gl,
                               GtkWidget *window){
	int index = 0;
	
	/* Set buttons   */

    
	GtkWidget *switch_axis = gtk_switch_new();
	if(kemoview_get_object_property_flags(kemo_gl->kemoview_data,
                                          AXIS_TOGGLE) == 0){
		gtk_switch_set_active(GTK_SWITCH(switch_axis), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(switch_axis), TRUE);
	};
	g_signal_connect(G_OBJECT(switch_axis), "notify::active",
				G_CALLBACK(draw_axis_switch_CB), (gpointer) kemo_gl);
	
	GtkWidget *switch_coastline = gtk_switch_new();
	if(kemoview_get_object_property_flags(kemo_gl->kemoview_data,
                                          COASTLINE_SWITCH) == 0){
		gtk_switch_set_active(GTK_SWITCH(switch_coastline), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(switch_coastline), TRUE);
	};
	g_signal_connect(G_OBJECT(switch_coastline), "notify::active",
				G_CALLBACK(draw_coastline_switch_CB), (gpointer) kemo_gl);
	
	GtkWidget *switch_sph_grid = gtk_switch_new();
	if(kemoview_get_object_property_flags(kemo_gl->kemoview_data,
                                          SPHEREGRID_SWITCH) == 0){
		gtk_switch_set_active(GTK_SWITCH(switch_sph_grid), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(switch_sph_grid), TRUE);
	};
	g_signal_connect(G_OBJECT(switch_sph_grid), "notify::active",
				G_CALLBACK(draw_sph_grid_switch_CB), (gpointer) kemo_gl);
	
	GtkAdjustment *adj_coast_radius
        = gtk_adjustment_new(kemoview_get_coastline_radius(kemo_gl->kemoview_data),
                             0.0, 10.0, 0.02, 0.02, 0.0);
	GtkWidget *spin_coast_radius = gtk_spin_button_new(GTK_ADJUSTMENT(adj_coast_radius), 0, 3);
	g_signal_connect(spin_coast_radius, "value-changed",
                     G_CALLBACK(coastline_radius_CB), (gpointer) kemo_gl);

    
    GtkWidget *switch_tangent_cyl = gtk_switch_new();
    if(kemoview_get_object_property_flags(kemo_gl->kemoview_data,
                                          TANGENT_CYLINDER_SWITCH) == 0){
        gtk_switch_set_active(GTK_SWITCH(switch_tangent_cyl), FALSE);
    } else {
        gtk_switch_set_active(GTK_SWITCH(switch_tangent_cyl), TRUE);
    };
    g_signal_connect(G_OBJECT(switch_tangent_cyl), "notify::active",
                G_CALLBACK(draw_tangent_cyl_switch_CB), (gpointer) kemo_gl);
    

    GtkAdjustment *adj_ICB_radius
        = gtk_adjustment_new(kemoview_get_inner_core_radius(kemo_gl->kemoview_data),
                             0.0, 10.0, 0.02, 0.02, 0.0);
    GtkWidget *spin_ICB_radius = gtk_spin_button_new(GTK_ADJUSTMENT(adj_ICB_radius), 0, 3);
    g_signal_connect(spin_ICB_radius, "value-changed",
                     G_CALLBACK(ICB_radius_CB), (gpointer) kemo_gl);

	GtkWidget *hbox_axis = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_axis), gtk_label_new("Draw axis: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_axis), switch_axis, FALSE, FALSE, 0);
	
	GtkWidget *hbox_coastline = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_coastline), gtk_label_new("Draw coastline: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_coastline), switch_coastline, FALSE, FALSE, 0);
	
	GtkWidget *hbox_sph_grid = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_sph_grid), gtk_label_new("Draw sphere grid: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_sph_grid), switch_sph_grid, FALSE, FALSE, 0);
	
	GtkWidget *hbox_coast_radius = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_coast_radius), gtk_label_new("Radius: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_coast_radius), spin_coast_radius, FALSE, FALSE, 0);
	
    GtkWidget *hbox_tangent_cyl = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_tangent_cyl), gtk_label_new("Draw tangent cylinder: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_tangent_cyl), switch_tangent_cyl, FALSE, FALSE, 0);
    
    GtkWidget *hbox_ICB_radius = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_ICB_radius), gtk_label_new("ICB Radius: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_ICB_radius), spin_ICB_radius, FALSE, FALSE, 0);
    

	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_axis, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_coastline, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_sph_grid, TRUE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_coast_radius, TRUE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox), hbox_tangent_cyl, TRUE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox), hbox_ICB_radius, TRUE, FALSE, 0);

    return wrap_into_scroll_expansion_gtk("Axis and grids", 200, 240, window, vbox);
}
