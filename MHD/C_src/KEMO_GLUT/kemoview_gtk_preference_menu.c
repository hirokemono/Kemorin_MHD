/*
 *  kemoview_gtk_preference_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_preference_menu.h"

GtkWidget *window_BGCmap;
GtkWidget *window_pref;

static void set_background_GTK(GtkColorChooser *colordialog)
{
	GdkRGBA gcolor;
	GLfloat color[4];
	
	gtk_color_chooser_get_rgba(colordialog, &gcolor);
	gtk_widget_destroy(window_BGCmap);
	
    color[0] = (GLfloat) gcolor.red;
    color[1] = (GLfloat) gcolor.green;
    color[2] = (GLfloat) gcolor.blue;
	/*printf("New background Color (R,G,B): %.7e %.7e %.7e \n", color[0], color[1], color[2]);*/
	
	kemoview_set_background_color(color);
	
	return;
}

static void kemoview_BG_close(GtkButton *button, gpointer data){
	gtk_widget_destroy(window_pref);
	gtk_main_quit();
	return;
};

static void kemoview_gtk_BGcolorsel(GtkButton *button, gpointer data){
	int response;
	GtkColorChooser *chooser;
	GtkWindow *parent;
	
	parent = GTK_WINDOW(g_object_get_data(G_OBJECT(data), "parent"));
	
	window_BGCmap = gtk_color_chooser_dialog_new("Choose color", parent);
	gtk_widget_show_all(window_BGCmap);
	
	response = gtk_dialog_run(GTK_DIALOG(window_BGCmap));
	if (response == GTK_RESPONSE_OK){
		chooser = GTK_COLOR_CHOOSER(window_BGCmap);
		set_background_GTK(chooser);
		g_print ("color selected \n");
	}
	else if( response == GTK_RESPONSE_CANCEL ){
		g_print( "Cancel button was pressed.\n" );
		gtk_widget_destroy(window_BGCmap);
	}
	return;
}

static void AmbientChange(GtkWidget *entry, gpointer data)
{
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemovier_set_material_ambient(value);
/*	printf("gtk_min %d\n", gtk_min);*/
}
static void DiffuseChange(GtkWidget *entry, gpointer data)
{
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_material_diffuse(value);
/*	printf("gtk_min %d\n", gtk_min);*/
}
static void SpecularChange(GtkWidget *entry, gpointer data)
{
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_material_specular(value);
/*	printf("gtk_min %d\n", gtk_min);*/
}
static void ShinenessChange(GtkWidget *entry, gpointer data)
{
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	struct kemoviewer_type *kv_data = (struct kemoviewer_type *) data;
	
	kemoview_set_material_shineness(value);
/*	printf("gtk_min %d\n", gtk_min);*/
}

void gtk_BGcolorselect(const char *title, struct kemoviewer_type *kemoviewer_data){
	GtkWidget *label01, *label02, *label03, *label04;
	GtkWidget *label11, *label12, *label13, *label14;
	GtkWidget *label21, *label22, *label23, *label24;
	
	GtkWidget *hbox, *vbox;
	
	GtkWidget *hbox11, *hbox12, *hbox13, *hbox14;
	GtkWidget *entry;
	GtkWidget *BGselButton, *CloseButton;
	GtkWidget *spin1, *spin2, *spin3, *spin4;
	GtkAdjustment *adj1, *adj2, *adj3, *adj4;
	
    struct lightparams_view *lightparams_view;
	char current_text[30];
	float current_value;
	
    GLfloat color[4];
	kemoview_get_background_color(color);
	
	window_pref = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	
	gtk_window_set_title(GTK_WINDOW(window_pref), title);
	gtk_widget_set_size_request(window_pref, 150, -1);
	gtk_container_set_border_width(GTK_CONTAINER(window_pref), 5);
	g_signal_connect(G_OBJECT(window_pref), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	
	vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_container_add(GTK_CONTAINER(window_pref), vbox);
	
	/* Set buttons   */
	entry = gtk_entry_new();
	BGselButton = gtk_button_new_with_label("Set Background");
	g_signal_connect(G_OBJECT(BGselButton), "clicked", 
				G_CALLBACK(kemoview_gtk_BGcolorsel), (gpointer)entry);
	CloseButton = gtk_button_new_with_label("Close");
	g_signal_connect(G_OBJECT(CloseButton), "clicked", 
				G_CALLBACK(kemoview_BG_close), (gpointer)entry);
	
	lightparams_view = (struct lightparams_view *) malloc(sizeof(struct lightparams_view));
    init_light_views_4_viewer(kemoviewer_data->kemo_shaders->lights, lightparams_view);
	
	
	label01 = gtk_label_new("Current ambient");
	label02 = gtk_label_new("Current diffuse");
	label03 = gtk_label_new("Current specular");
	label04 = gtk_label_new("Current shineness");
	
	
	current_value = kemoview_send_material_ambient();
	sprintf(current_text, "    %e    ", current_value);
	label21 = gtk_label_new("Ambient:   ");
	adj1 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
	label11 = gtk_label_new(current_text);
	spin1 = gtk_spin_button_new(GTK_ADJUSTMENT(adj1),0,2);
	
	current_value = kemoview_send_material_diffuse();
	sprintf(current_text, "    %e    ", current_value);
	label22 = gtk_label_new("Diffuse:   ");
	label12 = gtk_label_new(current_text);
	adj2 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
	spin2 = gtk_spin_button_new(GTK_ADJUSTMENT(adj2),0,2);
	
	current_value = kemoview_send_material_specular();
	sprintf(current_text, "    %e    ", current_value);
	label23 = gtk_label_new("Specular:  ");
	label13 = gtk_label_new(current_text);
	adj3 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
	spin3 = gtk_spin_button_new(GTK_ADJUSTMENT(adj3),0,2);
	
	current_value = kemoview_send_material_shiness();
	sprintf(current_text, "    %e    ", current_value);
	label24 = gtk_label_new("Shineness: ");
	label14 = gtk_label_new(current_text);
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
	
	
	gtk_box_pack_start(GTK_BOX(vbox), BGselButton, FALSE, FALSE, 0);
	add_light_list_box(lightparams_view, vbox);
	
	gtk_box_pack_start(GTK_BOX(vbox), label01, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), label11, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox11, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), label02, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), label12, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox12, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), label03, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), label13, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox13, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), label04, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), label14, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox14, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), CloseButton, FALSE, FALSE, 0);
	
	
	gtk_widget_show_all(window_pref);
	gtk_main();
	dealloc_light_views_4_viewer(lightparams_view);
	return;
}
