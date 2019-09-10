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
	
	draw_mesh_glfw();
	return;
}

static void AmbientChange(GtkWidget *entry, gpointer data)
{
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemovier_set_material_ambient(value);
/*	printf("gtk_min %d\n", gtk_min);*/
	
	draw_mesh_glfw();
	return;
}
static void DiffuseChange(GtkWidget *entry, gpointer data)
{
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_material_diffuse(value);
/*	printf("gtk_min %d\n", gtk_min);*/
	
	draw_mesh_glfw();
	return;
}
static void SpecularChange(GtkWidget *entry, gpointer data)
{
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_material_specular(value);
/*	printf("gtk_min %d\n", gtk_min);*/
	
	draw_mesh_glfw();
	return;
}
static void ShinenessChange(GtkWidget *entry, gpointer data)
{
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	struct kemoviewer_type *kv_data = (struct kemoviewer_type *) data;
	
	kemoview_set_material_shineness(value);
/*	printf("gtk_min %d\n", gtk_min);*/
	
	draw_mesh_glfw();
	return;
}

void kemoview_preference_GTK(struct kemoviewer_type *kemoviewer_data, 
			struct lightparams_view *lightparams_vws, GtkWidget *box_out){
	GtkWidget *label21, *label22, *label23, *label24;
	
	GtkWidget *hbox, *vbox;
	
	GtkWidget *hbox11, *hbox12, *hbox13, *hbox14;
	GtkWidget *entry;
	GtkWidget *BGselButton;
	GtkWidget *spin1, *spin2, *spin3, *spin4;
	GtkAdjustment *adj1, *adj2, *adj3, *adj4;
	
	float current_value;
	
    GLfloat color[4];
	kemoview_get_background_color(color);
    init_light_views_4_viewer(kemoviewer_data->kemo_shaders->lights, lightparams_vws);
	
	/* Set buttons   */
	entry = gtk_entry_new();
	BGselButton = gtk_button_new_with_label("Set Background");
	g_signal_connect(G_OBJECT(BGselButton), "clicked", 
				G_CALLBACK(kemoview_gtk_BGcolorsel), (gpointer)entry);
	
	
	current_value = kemoview_send_material_ambient();
	label21 = gtk_label_new("Ambient:   ");
	adj1 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
	spin1 = gtk_spin_button_new(GTK_ADJUSTMENT(adj1),0,2);
	
	current_value = kemoview_send_material_diffuse();
	label22 = gtk_label_new("Diffuse:   ");
	adj2 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
	spin2 = gtk_spin_button_new(GTK_ADJUSTMENT(adj2),0,2);
	
	current_value = kemoview_send_material_specular();
	label23 = gtk_label_new("Specular:  ");
	adj3 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
	spin3 = gtk_spin_button_new(GTK_ADJUSTMENT(adj3),0,2);
	
	current_value = kemoview_send_material_shiness();
	label24 = gtk_label_new("Shineness: ");
	adj4 = gtk_adjustment_new(current_value, 0.0, 100.0, 0.1, 0.1, 0.0);
	spin4 = gtk_spin_button_new( GTK_ADJUSTMENT(adj4),0,10.0);
	
	g_signal_connect(spin1, "value-changed", G_CALLBACK(AmbientChange), NULL);
	g_signal_connect(spin2, "value-changed", G_CALLBACK(DiffuseChange), NULL);
	g_signal_connect(spin3, "value-changed", G_CALLBACK(SpecularChange), NULL);
	g_signal_connect(spin4, "value-changed", G_CALLBACK(ShinenessChange),
				(gpointer) kemoviewer_data);
	
	hbox11 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	gtk_box_pack_start(GTK_BOX(hbox11), label21, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox11), spin1, FALSE, FALSE, 0);
	
	hbox12 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	gtk_box_pack_start(GTK_BOX(hbox12), label22, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox12), spin2, FALSE, FALSE, 0);
	
	hbox13 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	gtk_box_pack_start(GTK_BOX(hbox13), label23, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox13), spin3, FALSE, FALSE, 0);
	
	hbox14 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	gtk_box_pack_start(GTK_BOX(hbox14), label24, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox14), spin4, FALSE, FALSE, 0);
	
	
	vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox), BGselButton, FALSE, FALSE, 0);
	add_light_list_box(lightparams_vws, vbox);
	
	gtk_box_pack_start(GTK_BOX(vbox), hbox11, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox12, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox13, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox14, FALSE, FALSE, 0);
	
	wrap_into_expanded_frame_gtk("Preferences", vbox, box_out);
	
	gtk_widget_show_all(vbox);
	
	return;
}
