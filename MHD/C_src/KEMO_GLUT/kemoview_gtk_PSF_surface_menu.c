/*
 *  kemoview_gtk_PSF_surface_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_PSF_surface_menu.h"

static void set_psf_opacity_CB(GtkWidget *entry, gpointer user_data)
{
	struct colormap_view *color_vws = (struct colormap_view *) user_data;
	double gtk_floatvalue = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_PSF_constant_opacity(gtk_floatvalue);
	return;
}

static void MinChange(GtkWidget *entry, gpointer data)
{
	int icomp = kemoview_get_PSF_draw_data_address();
	double data_max = kemoview_get_PSF_max_data(icomp);
	
	double data_min = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_PSF_linear_colormap(data_min, data_max);
}
static void MaxChange(GtkWidget *entry, gpointer data)
{
	int icomp = kemoview_get_PSF_draw_data_address();
	double data_min = kemoview_get_PSF_min_data(icomp);
	
	double data_max = (double) gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_PSF_linear_colormap(data_min, data_max);
}

void add_gtk_psf_surface_menu(struct colormap_view *color_vws, GtkWidget *box){
	GtkWidget *hbox_one_opacity, *hbox_org_opacity;
	GtkWidget *hbox_range, *hbox_org_range;
	
	GtkWidget *expander_psf,  *scroll_psf, *Frame_psf;
	GtkWidget *hbox_psf,  *vbox_psf;
	
	GtkWidget *spin_opacity1;
	GtkAdjustment *adj_opacity1;
	double current_value;
	char current_text[30];
	
	GtkWidget *spin_min, *spin_max;
	GtkAdjustment *adj_min, *adj_max;
	int icomp;
	double data_min, data_max;
	double range_min, range_max, delta;
	char min_text[30], max_text[30];
	
	current_value = kemoview_get_PSF_max_opacity();
	sprintf(current_text, "    %e    ", current_value);
	adj_opacity1 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
	spin_opacity1 = gtk_spin_button_new(GTK_ADJUSTMENT(adj_opacity1), 0, 2);
	g_signal_connect(spin_opacity1, "value-changed", G_CALLBACK(set_psf_opacity_CB), color_vws);
	
	icomp = kemoview_get_PSF_draw_data_address();
	range_min = kemoview_get_PSF_min_data(icomp);
	range_max = kemoview_get_PSF_max_data(icomp);
	data_min = kemoview_get_PSF_color_table_min();
	data_max = kemoview_get_PSF_color_table_max();
	delta = range_max - range_min;
	sprintf(min_text, "    %e    ", range_min);
	sprintf(max_text, "    %e    ", range_max);
	adj_min = gtk_adjustment_new (data_min, (range_min-1.0e3*delta), (range_max+1.0e3*delta),
			(delta*1.0e-2), (delta*1.0e-2), 0.0);
	adj_max = gtk_adjustment_new (data_max, (range_min*1.0e3),  (range_max+1.0e3*delta),
			(delta*1.0e-2), (delta*1.0e-2), 0.0);
	spin_min = gtk_spin_button_new(GTK_ADJUSTMENT(adj_min),0,2);
	spin_max = gtk_spin_button_new(GTK_ADJUSTMENT(adj_max),0,2);
	g_signal_connect(spin_min, "value-changed", G_CALLBACK(MinChange), NULL);
	g_signal_connect(spin_max, "value-changed", G_CALLBACK(MaxChange), NULL);
	
	hbox_org_opacity = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_org_opacity), gtk_label_new("Current opacity: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_org_opacity), gtk_label_new(current_text), TRUE, TRUE, 0);
	hbox_one_opacity = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_one_opacity), gtk_label_new("Opacity: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_one_opacity), spin_opacity1, TRUE, TRUE, 0);
	
	hbox_org_range = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_org_range), gtk_label_new(min_text), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_org_range), gtk_label_new(max_text), TRUE, TRUE, 0);
	hbox_range = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_range), spin_min, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_range), spin_max, TRUE, TRUE, 0);
	
	vbox_psf = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_box_pack_start(GTK_BOX(vbox_psf), hbox_org_opacity, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_psf), hbox_one_opacity, TRUE, TRUE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox_psf), gtk_label_new("Range"), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_psf), hbox_org_range, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_psf), hbox_range, TRUE, TRUE, 0);
	
	Frame_psf = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_psf), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame_psf), vbox_psf);
	
	hbox_psf = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_psf), gtk_label_new("  "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_psf), Frame_psf, TRUE, TRUE, 0);
	
	scroll_psf = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll_psf),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scroll_psf, 400, 300);
	gtk_container_add(GTK_CONTAINER(scroll_psf), hbox_psf);
	
	expander_psf = gtk_expander_new_with_mnemonic("Surface");
	gtk_container_add(GTK_CONTAINER(expander_psf), scroll_psf);
	
	gtk_box_pack_start(GTK_BOX(box), expander_psf, TRUE, FALSE, 0);
	return;
}
