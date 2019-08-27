/*
 *  kemoview_gtk_colormap_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_colormap_menu.h"

GtkWidget *window_cmap;
GtkWidget *window_pref;

static void cb_close_window(GtkButton *button, gpointer user_data){
    GtkWidget *window = (GtkWidget *) user_data;
    gtk_widget_destroy(window);
};

static void save_colormap_file_panel(GtkButton *saveButton, gpointer user_data){
	struct colormap_view *color_vws = (struct colormap_view *) user_data;
	struct kv_string *filename = kemoview_save_file_panel(window_cmap);
	
	if(filename->string[0] != '\0'){
		write_colormap_control_file_s(filename->string, color_vws->cmap_param);
	};
	kemoview_free_kvstring(filename);
	return;
};
static void load_colormap_file_panel(GtkButton *loadButton, gpointer user_data){
	struct colormap_view *color_vws = (struct colormap_view *) user_data;
	struct kv_string *filename = kemoview_read_file_panel(window_cmap);
	
	if(filename->string[0] != '\0'){
		read_colormap_control_file_s(filename->string, color_vws->cmap_param);
	};
	kemoview_free_kvstring(filename);
	return;
};

static void set_nline_CB(GtkWidget *entry, gpointer user_data)
{
	struct colormap_view *color_vws = (struct colormap_view *) user_data;
	int gtk_intvalue = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
	kemoview_set_PSF_num_isoline(gtk_intvalue);
	return;
}

void add_gtk_isoline_menu(struct colormap_view *color_vws, GtkWidget *box){
	GtkWidget *hbox_22, *hbox_23;
	
	GtkWidget *expander_iso,  *scroll_iso, *Frame_iso;
	GtkWidget *hbox_iso,  *vbox_iso;
	
	GtkWidget *spin_nline;
	GtkAdjustment *adj_nline;
	char current_nline_txt[30];
	
	int current_nline = kemoview_get_PSF_num_isoline();
	sprintf(current_nline_txt, "    %d    ", current_nline);
	adj_nline = gtk_adjustment_new ((double) current_nline, 0, 200, 1, 1, 0.0);
	spin_nline = gtk_spin_button_new(GTK_ADJUSTMENT(adj_nline), 0, 2);
	g_signal_connect(spin_nline, "value-changed", G_CALLBACK(set_nline_CB), (gpointer) color_vws);
	
	hbox_23 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_23), gtk_label_new("Current num. of lines: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_23), gtk_label_new(current_nline_txt), TRUE, TRUE, 0);
	hbox_22 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_22), gtk_label_new("Num. of lines: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_22), spin_nline, TRUE, TRUE, 0);
	
	vbox_iso = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_box_pack_start(GTK_BOX(vbox_iso), hbox_23, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_iso), hbox_22, TRUE, TRUE, 0);
	
	Frame_iso = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_iso), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame_iso), vbox_iso);
	
	hbox_iso = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_iso), gtk_label_new("  "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_iso), Frame_iso, TRUE, TRUE, 0);
	
	scroll_iso = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll_iso),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scroll_iso, 400, 300);
	gtk_container_add(GTK_CONTAINER(scroll_iso), hbox_iso);
	
	expander_iso = gtk_expander_new_with_mnemonic("Isolines");
	gtk_container_add(GTK_CONTAINER(expander_iso), scroll_iso);
	
	gtk_box_pack_start(GTK_BOX(box), expander_iso, TRUE, FALSE, 0);
	return;
}

void add_gtk_psf_colormap_menu(struct colormap_view *color_vws, GtkWidget *box){
	GtkButton *saveButton, *loadButton;
	
	GtkWidget *expander_cmap, *scroll_cmap, *Frame_cmap;
	GtkWidget *hbox_cmap, *vbox_cmap;
	
	saveButton = gtk_button_new_with_label("Save colormap...");
	g_signal_connect(G_OBJECT(saveButton), "clicked", 
				G_CALLBACK(save_colormap_file_panel), G_OBJECT(color_vws));
	
	loadButton = gtk_button_new_with_label("Load colormap...");
	g_signal_connect(G_OBJECT(loadButton), "clicked", 
				G_CALLBACK(load_colormap_file_panel), G_OBJECT(color_vws));
	
	vbox_cmap = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	add_colormp_list_box(color_vws, vbox_cmap);
	gtk_box_pack_start(GTK_BOX(vbox_cmap), saveButton, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox_cmap), loadButton, FALSE, FALSE, 0);
	
	Frame_cmap = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_cmap), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame_cmap), vbox_cmap);
	
	hbox_cmap = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_cmap), gtk_label_new("  "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_cmap), Frame_cmap, TRUE, TRUE, 0);
	
	scroll_cmap = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll_cmap),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scroll_cmap, 400, 500);
	gtk_container_add(GTK_CONTAINER(scroll_cmap), hbox_cmap);
	
	expander_cmap = gtk_expander_new_with_mnemonic("Color map editor");
	gtk_container_add(GTK_CONTAINER(expander_cmap), scroll_cmap);
	
	gtk_box_pack_start(GTK_BOX(box), expander_cmap, TRUE, FALSE, 0);
	return;
}

void gtk_psf_colormap_menu(struct kv_string *title, 
			struct kemoviewer_type *kemoviewer_data){
	GtkWidget *hbox_22, *hbox_23;
	GtkWidget *box;
	GtkButton *closeButton;
	
	struct colormap_view *color_vws
			= (struct colormap_view *) malloc(sizeof(struct colormap_view));
	init_colormap_views_4_viewer(kemoviewer_data->psf_current_menu, color_vws);
	
	
	window_cmap = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window_cmap), title->string);
	
	g_signal_connect(window_cmap, "destroy", G_CALLBACK(gtk_main_quit), NULL);
	gtk_container_set_border_width(GTK_CONTAINER(window_cmap), 5);
	
	closeButton = gtk_button_new_with_label("Update");
	g_signal_connect(G_OBJECT(closeButton), "clicked", 
				G_CALLBACK(cb_close_window), window_cmap);
	
	box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_container_add(GTK_CONTAINER(window_cmap), box);
	add_gtk_isoline_menu(color_vws, box);
	add_gtk_psf_surface_menu(color_vws, window_cmap, box);
	add_gtk_psf_colormap_menu(color_vws, box);
	int if_psf = kemoview_get_PSF_field_id();
	int ncomp = kemoview_get_PSF_num_component(if_psf);
	if(ncomp == 3){
		add_gtk_psf_vector_menu(color_vws, box);
	};
	gtk_box_pack_start(GTK_BOX(box), closeButton, FALSE, FALSE, 0);
	
	gtk_widget_show_all(window_cmap);
	gtk_main();
	dealloc_colormap_views_4_viewer(color_vws);
	free(color_vws);
	return;
}

static void set_background_GTK(GtkColorChooser *colordialog)
{
	GdkRGBA gcolor;
	GLfloat color[4];
	
	gtk_color_chooser_get_rgba(colordialog, &gcolor);
	gtk_widget_destroy(window_cmap);
	
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
	
	window_cmap = gtk_color_chooser_dialog_new("Choose color", parent);
	gtk_widget_show_all(window_cmap);
	
	response = gtk_dialog_run(GTK_DIALOG(window_cmap));
	if (response == GTK_RESPONSE_OK){
		chooser = GTK_COLOR_CHOOSER(window_cmap);
		set_background_GTK(chooser);
		g_print ("color selected \n");
	}
	else if( response == GTK_RESPONSE_CANCEL ){
		g_print( "Cancel button was pressed.\n" );
		gtk_widget_destroy(window_cmap);
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
