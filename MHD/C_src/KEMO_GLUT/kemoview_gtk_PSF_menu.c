/*
 *  kemoview_gtk_PSF_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_PSF_menu.h"

static void save_colormap_file_panel_CB(GtkButton *saveButton, gpointer user_data){
	GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct colormap_view *color_vws = (struct colormap_view *) g_object_get_data(G_OBJECT(user_data), "colorview");
	struct kv_string *filename = kemoview_save_file_panel(window);
	
	if(filename->string[0] != '\0'){
		write_colormap_control_file_s(filename->string, color_vws->cmap_param);
	};
	kemoview_free_kvstring(filename);
	return;
};

static void load_colormap_file_panel_CB(GtkButton *loadButton, gpointer user_data){
	GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct colormap_view *color_vws = (struct colormap_view *) g_object_get_data(G_OBJECT(user_data), "colorview");
	struct kv_string *filename = kemoview_read_file_panel(window);
	
	if(filename->string[0] != '\0'){
		read_colormap_control_file_s(filename->string, color_vws->cmap_param);
	};
	kemoview_free_kvstring(filename);
	
	gtk_widget_queue_draw(window);
	draw_full();
	return;
};


static void add_gtk_psf_colormap_menu(GtkWidget *window, struct colormap_view *color_vws, 
							   struct psf_color_gtk_menu *psf_color_menu){
	GtkWidget *saveButton, *loadButton;
	GtkWidget *entry;
	
	entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer) window);
	g_object_set_data(G_OBJECT(entry), "colorview", (gpointer) color_vws);
	saveButton = gtk_button_new_with_label("Save colormap...");
	g_signal_connect(G_OBJECT(saveButton), "clicked", 
				G_CALLBACK(save_colormap_file_panel_CB), G_OBJECT(entry));
	
	loadButton = gtk_button_new_with_label("Load colormap...");
	g_signal_connect(G_OBJECT(loadButton), "clicked", 
				G_CALLBACK(load_colormap_file_panel_CB), G_OBJECT(entry));
	
	add_kemoview_colormap_list_box(color_vws, psf_color_menu->color_box);
	gtk_box_pack_start(GTK_BOX(psf_color_menu->color_box), saveButton, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(psf_color_menu->color_box), loadButton, FALSE, FALSE, 0);
	return;
}

struct psf_gtk_menu * alloc_psf_gtk_menu(){
	struct psf_gtk_menu *psf_gmenu = (struct psf_gtk_menu *) malloc(sizeof(struct psf_gtk_menu));
	
	psf_gmenu->color_vws = (struct colormap_view *) malloc(sizeof(struct colormap_view));
	psf_gmenu->psf_isoline_menu = (struct psf_isoline_gtk_menu *) malloc(sizeof(struct psf_isoline_gtk_menu));
	psf_gmenu->psf_surface_menu = (struct psf_surface_gtk_menu *) malloc(sizeof(struct psf_surface_gtk_menu));
	psf_gmenu->psf_color_menu =   (struct psf_color_gtk_menu *) malloc(sizeof(struct psf_color_gtk_menu));
	psf_gmenu->psf_vector_menu =  (struct psf_vector_gtk_menu *) malloc(sizeof(struct psf_vector_gtk_menu));
	
	return psf_gmenu;
};

void dealloc_psf_gtk_menu(struct psf_gtk_menu *psf_gmenu){
	free(psf_gmenu->psf_isoline_menu);
	free(psf_gmenu->psf_surface_menu);
	free(psf_gmenu->psf_color_menu);
	free(psf_gmenu->psf_vector_menu);
	
	dealloc_colormap_views_4_viewer(psf_gmenu->color_vws);
	free(psf_gmenu->color_vws);
	
	free(psf_gmenu);
	return;
};

void make_psf_menu_box(GtkWidget *window, struct psf_gtk_menu *psf_gmenu){
	int if_psf = kemoview_get_each_PSF_field_param(FIELD_SEL_FLAG);
	int ncomp = kemoview_get_PSF_num_component(if_psf);
	
	psf_gmenu->psf_isoline_menu->isoline_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	add_gtk_isoline_menu(window, psf_gmenu->psf_isoline_menu);
	set_gtk_isoline_menu_values(psf_gmenu->psf_isoline_menu);
	wrap_into_expanded_frame_gtk("Isolines", 425, 220, psf_gmenu->psf_isoline_menu->isoline_box, 
								 psf_gmenu->psf_vbox);

	psf_gmenu->psf_surface_menu->patch_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	add_gtk_psf_surface_menu(window, psf_gmenu->color_vws, psf_gmenu->psf_surface_menu);
	set_gtk_surface_menu_values(psf_gmenu->psf_surface_menu);
	wrap_into_expanded_frame_gtk("Surface", 420, 320, psf_gmenu->psf_surface_menu->patch_box,
								 psf_gmenu->psf_vbox);
	
	psf_gmenu->psf_color_menu->color_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	add_gtk_psf_colormap_menu(window, psf_gmenu->color_vws, psf_gmenu->psf_color_menu);
	wrap_into_expanded_frame_gtk("Color map editor", 420, 450, psf_gmenu->psf_color_menu->color_box,
								 psf_gmenu->psf_vbox);
	
	psf_gmenu->psf_vector_menu->vector_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	if(ncomp == 3){
		make_gtk_psf_vector_menu(psf_gmenu->color_vws, psf_gmenu->psf_vector_menu);
		set_gtk_psf_vector_menu(psf_gmenu->psf_vector_menu);
	};
	gtk_box_pack_start(GTK_BOX(psf_gmenu->psf_vbox), psf_gmenu->psf_vector_menu->vector_box,
					   FALSE, TRUE, 0);
	
	gtk_widget_show_all(psf_gmenu->psf_vbox);
	if(ncomp == 3){
		gtk_widget_show(psf_gmenu->psf_vector_menu->vector_box);
	} else {
		gtk_widget_hide(psf_gmenu->psf_vector_menu->vector_box);
	};
	return;
}

