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
	GtkWindow *parent = GTK_WINDOW(g_object_get_data(G_OBJECT(data), "parent"));
	
	int iflag_set = kemoview_gtk_colorsel_CB(parent, color);
	if(iflag_set > 0){kemoview_set_background_color(color);};
	
	draw_full();
	return;
}

static void AmbientChange(GtkWidget *entry, gpointer data)
{
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_material_parameter(AMBIENT_FLAG, value);
	draw_full();
	return;
}
static void DiffuseChange(GtkWidget *entry, gpointer data)
{
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_material_parameter(DIFFUSE_FLAG, value);
	draw_full();
	return;
}
static void SpecularChange(GtkWidget *entry, gpointer data)
{
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_material_parameter(SPECULAR_FLAG, value);
	draw_full();
	return;
}
static void ShinenessChange(GtkWidget *entry, gpointer data)
{
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	
	kemoview_set_material_parameter(SHINENESS_FLAG, value);
	draw_full();
	return;
}

void set_GTK_preference_menu(struct gtk_preference_menu *gtk_pref){
	double current_value;
	current_value = kemoview_get_material_parameter(AMBIENT_FLAG);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(gtk_pref->spin_ambient), current_value);
	current_value = kemoview_get_material_parameter(DIFFUSE_FLAG);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(gtk_pref->spin_diffuse), current_value);
	current_value = kemoview_get_material_parameter(SPECULAR_FLAG);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(gtk_pref->spin_specular), current_value);
	current_value = kemoview_get_material_parameter(SHINENESS_FLAG);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(gtk_pref->spin_shineness), current_value);
	return;
}

void add_GTK_preference_box(struct lightparams_view *lightparams_vws, struct gtk_preference_menu *gtk_pref, 
							GtkWidget *box_out){
	GtkWidget *vbox;
	
	GtkWidget *hbox11, *hbox12, *hbox13, *hbox14;
	GtkWidget *entry;
	GtkWidget *BGselButton;
	GtkAdjustment *adj1, *adj2, *adj3, *adj4;
	
	float current_value = 0.0;
	
    GLfloat color[4];
	kemoview_get_background_color(color);
	
	/* Set buttons   */
	entry = gtk_entry_new();
	BGselButton = gtk_button_new_with_label("Set Background");
	g_signal_connect(G_OBJECT(BGselButton), "clicked", 
				G_CALLBACK(kemoview_gtk_BGcolorsel), (gpointer)entry);
	
	
	adj1 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
	gtk_pref->spin_ambient = gtk_spin_button_new(GTK_ADJUSTMENT(adj1),0,2);
	
	adj2 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
	gtk_pref->spin_diffuse = gtk_spin_button_new(GTK_ADJUSTMENT(adj2),0,2);
	
	adj3 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
	gtk_pref->spin_specular = gtk_spin_button_new(GTK_ADJUSTMENT(adj3),0,2);
	
	adj4 = gtk_adjustment_new(current_value, 0.0, 100.0, 0.1, 0.1, 0.0);
	gtk_pref->spin_shineness = gtk_spin_button_new( GTK_ADJUSTMENT(adj4),0,2);
	
	g_signal_connect(gtk_pref->spin_ambient, "value-changed", G_CALLBACK(AmbientChange), NULL);
	g_signal_connect(gtk_pref->spin_diffuse, "value-changed", G_CALLBACK(DiffuseChange), NULL);
	g_signal_connect(gtk_pref->spin_specular, "value-changed", G_CALLBACK(SpecularChange), NULL);
	g_signal_connect(gtk_pref->spin_shineness, "value-changed", G_CALLBACK(ShinenessChange), NULL);
	
	set_GTK_preference_menu(gtk_pref);
	
	hbox11 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	gtk_box_pack_start(GTK_BOX(hbox11), gtk_label_new("Ambient:   "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox11), gtk_pref->spin_ambient, FALSE, FALSE, 0);
	
	hbox12 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	gtk_box_pack_start(GTK_BOX(hbox12), gtk_label_new("Diffuse:   "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox12), gtk_pref->spin_diffuse, FALSE, FALSE, 0);
	
	hbox13 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	gtk_box_pack_start(GTK_BOX(hbox13), gtk_label_new("Specular:  "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox13), gtk_pref->spin_specular, FALSE, FALSE, 0);
	
	hbox14 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	gtk_box_pack_start(GTK_BOX(hbox14), gtk_label_new("Shineness: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox14), gtk_pref->spin_shineness, FALSE, FALSE, 0);
	
	
	vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox), BGselButton, FALSE, FALSE, 0);
	add_light_list_box(lightparams_vws, vbox);
	
	gtk_box_pack_start(GTK_BOX(vbox), hbox11, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox12, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox13, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox14, FALSE, FALSE, 0);
	
	wrap_into_expanded_frame_gtk("Preferences", 360, 400, vbox, box_out);
	
	gtk_widget_show_all(vbox);
	
	return;
}
