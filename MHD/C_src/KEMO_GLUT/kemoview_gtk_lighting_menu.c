/*
 *  kemoview_gtk_lighting_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_lighting_menu.h"

static void AmbientChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_material_parameter(AMBIENT_FLAG, value, kemo_sgl);
    draw_full(kemo_sgl);
	return;
}
static void DiffuseChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_material_parameter(DIFFUSE_FLAG, value, kemo_sgl);
    draw_full(kemo_sgl);
	return;
}
static void SpecularChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_material_parameter(SPECULAR_FLAG, value, kemo_sgl);
    draw_full(kemo_sgl);
	return;
}
static void ShinenessChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	
	kemoview_set_material_parameter(SHINENESS_FLAG, value, kemo_sgl);
    draw_full(kemo_sgl);
	return;
}

static void light_chack_switch_CB(GObject *switch_bar, GParamSpec *pspec, gpointer data){
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    int iflag = gtk_switch_get_state(GTK_SWITCH(switch_bar));
    kemoview_set_view_integer(LIGHTING_CHECK, iflag, kemo_sgl);
    draw_full(kemo_sgl);
    return;
};

GtkWidget * init_lighting_frame(struct kemoviewer_type *kemo_sgl,
                                struct lightparams_view *lightparams_vws){
    GtkWidget * light_vbox;
    
    float color[4];
    kemoview_get_background_color(kemo_sgl, color);
    
    /* Set buttons   */
    float current_value = 0.0;
    GtkAdjustment *adj1 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
    GtkWidget * spin_ambient = gtk_spin_button_new(GTK_ADJUSTMENT(adj1),0,2);
	current_value = kemoview_get_material_parameter(kemo_sgl, AMBIENT_FLAG);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_ambient), current_value);
    g_signal_connect(spin_ambient, "value-changed",
                     G_CALLBACK(AmbientChange_CB), (gpointer) kemo_sgl);
    
    GtkWidget * hbox_ambient = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(hbox_ambient), gtk_label_new("Ambient:   "), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_ambient), spin_ambient, FALSE, FALSE, 0);
    
    
    GtkAdjustment *adj2 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
    GtkWidget * spin_diffuse = gtk_spin_button_new(GTK_ADJUSTMENT(adj2),0,2);
	current_value = kemoview_get_material_parameter(kemo_sgl, DIFFUSE_FLAG);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_diffuse), current_value);
    g_signal_connect(spin_diffuse, "value-changed",
                     G_CALLBACK(DiffuseChange_CB), (gpointer) kemo_sgl);
    
    GtkWidget * hbox_diffuse = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(hbox_diffuse), gtk_label_new("Diffuse:   "), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_diffuse), spin_diffuse, FALSE, FALSE, 0);
    
    
    GtkAdjustment *adj3 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
    GtkWidget * spin_specular = gtk_spin_button_new(GTK_ADJUSTMENT(adj3),0,2);
	current_value = kemoview_get_material_parameter(kemo_sgl, SPECULAR_FLAG);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_specular), current_value);
    g_signal_connect(spin_specular, "value-changed",
                     G_CALLBACK(SpecularChange_CB), (gpointer) kemo_sgl);
    
    GtkWidget * hbox_specular = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(hbox_specular), gtk_label_new("Specular:  "), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_specular), spin_specular, FALSE, FALSE, 0);
    
    
    GtkAdjustment *adj4 = gtk_adjustment_new(current_value, 0.0, 100.0, 0.1, 0.1, 0.0);
    GtkWidget * spin_shineness = gtk_spin_button_new( GTK_ADJUSTMENT(adj4),0,2);
	current_value = kemoview_get_material_parameter(kemo_sgl, SHINENESS_FLAG);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_shineness), current_value);
    g_signal_connect(spin_shineness, "value-changed",
                     G_CALLBACK(ShinenessChange_CB), (gpointer) kemo_sgl);
    
    GtkWidget * hbox_shine = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(hbox_shine), gtk_label_new("Shineness: "), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_shine), spin_shineness, FALSE, FALSE, 0);

    
    
    GtkWidget * lighting_switch = gtk_switch_new();
    int iflag = kemoview_get_view_integer(kemo_sgl, LIGHTING_CHECK);
    gtk_switch_set_active(GTK_SWITCH(lighting_switch), (gboolean) iflag);
    g_signal_connect(G_OBJECT(lighting_switch), "notify::active",
                     G_CALLBACK(light_chack_switch_CB), (gpointer) kemo_sgl);

    GtkWidget * hbox_check = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(hbox_check), gtk_label_new("Check light directions: "), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_check), lighting_switch, FALSE, FALSE, 0);

    
    GtkWidget * Frame_lights = init_lightposition_expander(kemo_sgl, lightparams_vws);

    light_vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_pack_start(GTK_BOX(light_vbox), Frame_lights, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(light_vbox), hbox_ambient, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(light_vbox), hbox_diffuse, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(light_vbox), hbox_specular, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(light_vbox), hbox_shine, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(light_vbox), hbox_check, FALSE, FALSE, 0);

    return  wrap_into_frame_gtk("Light parameters", light_vbox);
}
