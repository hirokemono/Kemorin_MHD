/*
 *  kemoview_gtk_preference_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_preference_menu.h"

struct preference_gtk_menu * init_preference_gtk_menu(struct kemoviewer_type *kemoviewer_data){
	struct preference_gtk_menu *pref_gmenu
			= (struct preference_gtk_menu *) malloc(sizeof(struct preference_gtk_menu));
	pref_gmenu->lightparams_vws = init_light_views_4_viewer(kemoviewer_data->kemo_buffers->kemo_lights);
	
	return pref_gmenu;
};
void dealloc_preference_gtk_menu(struct preference_gtk_menu *pref_gmenu){
	dealloc_light_views_4_viewer(pref_gmenu->lightparams_vws);
	free(pref_gmenu);
	return;
};

static void kemoview_gtk_BGcolorsel(GtkButton *button, gpointer data){
	float color[4];
	GtkWindow *window = GTK_WINDOW(data);
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(data), "kemoview");

	int iflag_set = kemoview_gtk_colorsel_CB(window, color);
	if(iflag_set > 0){
        kemoview_set_background_color(color, kemo_sgl);
        kemoview_gl_background_color(kemo_sgl);
    };
	
    draw_full(kemo_sgl);
	return;
}

static void AmbientChange(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_material_parameter(AMBIENT_FLAG, value, kemo_sgl);
    draw_full(kemo_sgl);
	return;
}
static void DiffuseChange(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_material_parameter(DIFFUSE_FLAG, value, kemo_sgl);
    draw_full(kemo_sgl);
	return;
}
static void SpecularChange(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_material_parameter(SPECULAR_FLAG, value, kemo_sgl);
    draw_full(kemo_sgl);
	return;
}
static void ShinenessChange(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	
	kemoview_set_material_parameter(SHINENESS_FLAG, value, kemo_sgl);
    draw_full(kemo_sgl);
	return;
}

static void set_GTK_preference_menu(struct kemoviewer_type *kemoviewer_data,
                                    struct preference_gtk_menu *pref_gmenu){
	double current_value;
	current_value = kemoview_get_material_parameter(kemoviewer_data, AMBIENT_FLAG);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(pref_gmenu->spin_ambient), current_value);
	current_value = kemoview_get_material_parameter(kemoviewer_data, DIFFUSE_FLAG);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(pref_gmenu->spin_diffuse), current_value);
	current_value = kemoview_get_material_parameter(kemoviewer_data, SPECULAR_FLAG);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(pref_gmenu->spin_specular), current_value);
	current_value = kemoview_get_material_parameter(kemoviewer_data, SHINENESS_FLAG);
	gtk_spin_button_set_value(GTK_SPIN_BUTTON(pref_gmenu->spin_shineness), current_value);
	return;
}

GtkWidget * init_preference_expander(struct kemoviewer_type *kemo_sgl,
                                     struct preference_gtk_menu *pref_gmenu, GtkWidget *window,
                                     struct kemoviewer_type *kemoviewer_data){
    GtkWidget *expander_pref;

    float color[4];
	kemoview_get_background_color(kemo_sgl, color);
	
	/* Set buttons   */
    pref_gmenu->BGselButton = gtk_button_new_with_label("Set Background");
    g_object_set_data(G_OBJECT(window), "kemoview", (gpointer) kemoviewer_data);
	g_signal_connect(G_OBJECT(pref_gmenu->BGselButton), "clicked",
				G_CALLBACK(kemoview_gtk_BGcolorsel), (gpointer)window);

    pref_gmenu->Frame_BGsel = init_light_list_frame(kemo_sgl, pref_gmenu->lightparams_vws);
	
	
    float current_value = 0.0;
	GtkAdjustment *adj1 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
	pref_gmenu->spin_ambient = gtk_spin_button_new(GTK_ADJUSTMENT(adj1),0,2);
	
	GtkAdjustment *adj2 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
	pref_gmenu->spin_diffuse = gtk_spin_button_new(GTK_ADJUSTMENT(adj2),0,2);
	
	GtkAdjustment *adj3 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
	pref_gmenu->spin_specular = gtk_spin_button_new(GTK_ADJUSTMENT(adj3),0,2);
	
	GtkAdjustment *adj4 = gtk_adjustment_new(current_value, 0.0, 100.0, 0.1, 0.1, 0.0);
	pref_gmenu->spin_shineness = gtk_spin_button_new( GTK_ADJUSTMENT(adj4),0,2);
	
	g_signal_connect(pref_gmenu->spin_ambient, "value-changed",
                     G_CALLBACK(AmbientChange), (gpointer) kemo_sgl);
	g_signal_connect(pref_gmenu->spin_diffuse, "value-changed",
                     G_CALLBACK(DiffuseChange), (gpointer) kemo_sgl);
	g_signal_connect(pref_gmenu->spin_specular, "value-changed",
                     G_CALLBACK(SpecularChange), (gpointer) kemo_sgl);
	g_signal_connect(pref_gmenu->spin_shineness, "value-changed",
                     G_CALLBACK(ShinenessChange), (gpointer) kemo_sgl);
	
	set_GTK_preference_menu(kemoviewer_data, pref_gmenu);
	
    pref_gmenu->pref_hbox[0] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_hbox[0]), gtk_label_new("Ambient:   "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_hbox[0]), pref_gmenu->spin_ambient, FALSE, FALSE, 0);
	
    pref_gmenu->pref_hbox[1] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_hbox[1]), gtk_label_new("Diffuse:   "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_hbox[1]), pref_gmenu->spin_diffuse, FALSE, FALSE, 0);
	
    pref_gmenu->pref_hbox[2] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_hbox[2]), gtk_label_new("Specular:  "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_hbox[2]), pref_gmenu->spin_specular, FALSE, FALSE, 0);
	
    pref_gmenu->pref_hbox[3] = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_hbox[3]), gtk_label_new("Shineness: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_hbox[3]), pref_gmenu->spin_shineness, FALSE, FALSE, 0);
		
    pref_gmenu->pref_vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), pref_gmenu->BGselButton, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), pref_gmenu->Frame_BGsel, TRUE, TRUE, 0);
	
	gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), pref_gmenu->pref_hbox[0], FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), pref_gmenu->pref_hbox[1], FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), pref_gmenu->pref_hbox[2], FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), pref_gmenu->pref_hbox[3], FALSE, FALSE, 0);
	
    expander_pref = wrap_into_scroll_expansion_gtk("Preferences", 160, 400,
                                                   window, pref_gmenu->pref_vbox);
	return expander_pref;
}
