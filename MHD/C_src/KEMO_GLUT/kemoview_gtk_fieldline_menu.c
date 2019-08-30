/*
 *  kemoview_gtk_fieldline_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_fieldline_menu.h"

GtkWidget *window_fline;


static void close_fline_CB(GtkButton *button, gpointer user_data){
	int nload_psf;
	GtkWidget *window = (GtkWidget *) user_data;
	kemoview_close_fieldline_view();
};

static void close_window_CB(GtkButton *button, gpointer user_data){
    GtkWidget *window = (GtkWidget *) user_data;
    gtk_widget_destroy(window);
	gtk_main_quit();
};

static void fline_thickness_CB(GtkWidget *entry, gpointer data)
{
	double thick_in = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	if(thick_in > 0) kemoview_set_fline_thickness(thick_in);
/*	printf("thick_in %d\n", thick_in);*/
}

static void MinChange_CB(GtkWidget *entry, gpointer data)
{
	int icomp = kemoview_get_fline_color_data_adress();
	double data_max = kemoview_get_fline_max_color();
	
	double data_min = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_fline_linear_colormap(data_min, data_max);
}
static void MaxChange_CB(GtkWidget *entry, gpointer data)
{
	int icomp = kemoview_get_fline_color_data_adress();
	double data_min = kemoview_get_fline_min_color();
	
	double data_max = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_fline_linear_colormap(data_min, data_max);
}

static void psf_fieldtube_switch_CB(GObject *switch_1, GParamSpec *pspec, gpointer data){
	kemoview_toggle_fline_type();
	return;
};

void set_fline_thick_gtk(){
	GtkWidget *box;
	GtkButton *closeButton, *updateButton;
	
	GtkWidget *hbox_tube, *hbox_color;
	GtkWidget *hbox_thickness, *hbox_org_thick;
	GtkWidget *hbox_range, *hbox_org_range;
	
	GtkWidget *switch_tube;
	
	GtkWidget *spin_thick;
	GtkAdjustment *adj_thick;
	double current_thick;
	char current_thick_text[30];
	
	GtkWidget *spin_min, *spin_max;
	GtkAdjustment *adj_min, *adj_max;
	int ifield, icomp;
	double data_min, data_max;
	double range_min, range_max, delta;
	char min_text[30], max_text[30];
	
	
	window_fline = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window_fline), "Field lines");

	g_signal_connect(window_fline, "destroy", G_CALLBACK(gtk_main_quit), NULL);
	gtk_container_set_border_width(GTK_CONTAINER(window_fline), 5);

	closeButton = gtk_button_new_with_label("Close Current PSF");
	g_signal_connect(G_OBJECT(closeButton), "clicked", 
				G_CALLBACK(close_fline_CB), window_fline);
	
	updateButton = gtk_button_new_with_label("Update");
	g_signal_connect(G_OBJECT(updateButton), "clicked", 
				G_CALLBACK(close_window_CB), window_fline);
	
	
	switch_tube = gtk_switch_new();
	if(kemoview_get_fline_type() == 0){
		gtk_switch_set_active(GTK_SWITCH(switch_tube), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(switch_tube), TRUE);
	};
	g_signal_connect(G_OBJECT(switch_tube), "notify::active",
				G_CALLBACK(psf_fieldtube_switch_CB), NULL);
	
	
	current_thick = kemoview_get_fline_thickness();
	sprintf(current_thick_text, "    %e    ", current_thick);
	adj_thick = gtk_adjustment_new(current_thick, 0.0, 1.0, 0.005, 0.005, 0.0);
	spin_thick = gtk_spin_button_new(GTK_ADJUSTMENT(adj_thick), 0, 3);
	g_signal_connect(spin_thick, "value-changed", G_CALLBACK(fline_thickness_CB),NULL);
	
	ifield = kemoview_get_fline_color_field();
	icomp = kemoview_get_fline_color_data_adress();
	
	struct kv_string *colorname = kemoview_alloc_kvstring();
	kemoview_get_fline_color_data_name(colorname, ifield);
	
    range_min = kemoview_get_fline_data_min(icomp);
	range_max = kemoview_get_fline_data_max(icomp);
	data_min = kemoview_get_fline_min_color();
	data_max = kemoview_get_fline_max_color();
	
	delta = range_max - range_min;
	sprintf(min_text, "    %e    ", range_min);
	sprintf(max_text, "    %e    ", range_max);
	adj_min = gtk_adjustment_new (data_min, (range_min-1.0e3*delta), (range_max+1.0e3*delta),
			(delta*1.0e-2), (delta*1.0e-2), 0.0);
	adj_max = gtk_adjustment_new (data_max, (range_min*1.0e3*delta),  (range_max+1.0e3*delta),
			(delta*1.0e-2), (delta*1.0e-2), 0.0);
	spin_min = gtk_spin_button_new(GTK_ADJUSTMENT(adj_min),0,2);
	spin_max = gtk_spin_button_new(GTK_ADJUSTMENT(adj_max),0,2);
	g_signal_connect(spin_min, "value-changed", G_CALLBACK(MinChange_CB), NULL);
	g_signal_connect(spin_max, "value-changed", G_CALLBACK(MaxChange_CB), NULL);
	
	
	hbox_tube = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_tube), gtk_label_new("Draw tube: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_tube), switch_tube, FALSE, FALSE, 0);
	
	hbox_org_thick = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_org_thick), gtk_label_new("Current thickness: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_org_thick), gtk_label_new(current_thick_text), TRUE, TRUE, 0);
	hbox_thickness = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_thickness), gtk_label_new("Thickness: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_thickness), spin_thick, TRUE, TRUE, 0);
	
	hbox_org_range = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_org_range), gtk_label_new(min_text), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_org_range), gtk_label_new(max_text), TRUE, TRUE, 0);
	hbox_range = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_range), spin_min, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range), spin_max, TRUE, TRUE, 0);
	
	
	box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_container_add(GTK_CONTAINER(window_fline), box);
	gtk_box_pack_start(GTK_BOX(box), hbox_tube, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box), hbox_org_thick, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(box), hbox_thickness, TRUE, TRUE, 0);
	
	gtk_box_pack_start(GTK_BOX(box), gtk_label_new("Range"), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(box), hbox_org_range, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(box), hbox_range, TRUE, TRUE, 0);
	
	gtk_box_pack_start(GTK_BOX(box), closeButton, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box), updateButton, FALSE, FALSE, 0);
	
	gtk_widget_show_all(window_fline);
	gtk_main();

	return;
}
