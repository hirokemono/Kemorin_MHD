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

static void NumThreadsChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    int ivalue = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
    kemoview_set_number_of_threads(ivalue, kemo_sgl);
    draw_full(kemo_sgl);
    return;
}

static void nTubeCornerChange_CB(GtkWidget *entry, gpointer data)
{
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) data;
    int ivalue = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
    kemoview_set_view_integer(NUM_TUBE_CORNERS_FLAG, ivalue, kemo_sgl);
    draw_full(kemo_sgl);
    return;
}

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

static void rot_FPS_view_CB(GtkButton *button, gpointer user_data){
    GtkEntry *FPSentry = GTK_ENTRY(user_data);
    GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
    struct rotation_gtk_menu *rot_gmenu
            = (struct rotation_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "rotation");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");

    struct kv_string *image_prefix = kemoview_init_kvstring_by_string("CalypsoView");
    
    gtk_window_set_focus(GTK_WINDOW(window), NULL);
    double AverageFPS = draw_rotate_gl_views(kemo_sgl, NO_SAVE_FILE, image_prefix,
                                             rot_gmenu->iaxis_rot, rot_gmenu->inc_deg,
                                             2);
    kemoview_free_kvstring(image_prefix);


    gchar text_AverageFPS[25];
    sprintf(text_AverageFPS, "%f", (float) AverageFPS);
    gtk_entry_set_text(GTK_ENTRY(FPSentry), text_AverageFPS);
    return;
};



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

static void init_preference_vbox(struct kemoviewer_type *kemoviewer_data,
                                 struct rotation_gtk_menu *rot_gmenu,
                                 struct preference_gtk_menu *pref_gmenu,
                                 GtkWidget *window){
    float color[4];
	kemoview_get_background_color(kemoviewer_data, color);
	
	/* Set buttons   */
    pref_gmenu->BGselButton = gtk_button_new_with_label("Set Background");
    g_object_set_data(G_OBJECT(window), "kemoview", (gpointer) kemoviewer_data);
	g_signal_connect(G_OBJECT(pref_gmenu->BGselButton), "clicked",
                     G_CALLBACK(kemoview_gtk_BGcolorsel), (gpointer)window);

    
    int current_int = kemoview_get_number_of_threads(kemoviewer_data);
    GtkAdjustment *adj_t = gtk_adjustment_new(current_int, 1, 256, 1, 1, 0.0);
    pref_gmenu->spin_threads = gtk_spin_button_new(GTK_ADJUSTMENT(adj_t),0,2);
    g_signal_connect(pref_gmenu->spin_threads, "value-changed",
                     G_CALLBACK(NumThreadsChange_CB), (gpointer) kemoviewer_data);

    pref_gmenu->nthread_hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->nthread_hbox), gtk_label_new("# of threads:  "), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->nthread_hbox), pref_gmenu->spin_threads, FALSE, FALSE, 0);

    
    int ncorner = kemoview_get_view_integer(kemoviewer_data, NUM_TUBE_CORNERS_FLAG);
    GtkAdjustment *adj_c = gtk_adjustment_new(ncorner, 1, 24, 1, 1, 0.0);
    pref_gmenu->spin_nTubeCorner = gtk_spin_button_new(GTK_ADJUSTMENT(adj_c),0,2);
    g_signal_connect(pref_gmenu->spin_nTubeCorner, "value-changed",
                     G_CALLBACK(nTubeCornerChange_CB), (gpointer) kemoviewer_data);

    pref_gmenu->nTubeCorner_hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->nTubeCorner_hbox), gtk_label_new("# of tube corners:  "), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->nTubeCorner_hbox), pref_gmenu->spin_nTubeCorner, FALSE, FALSE, 0);

    
    pref_gmenu->fpsTextBox = gtk_entry_new();
    gtk_entry_set_text(GTK_ENTRY(pref_gmenu->fpsTextBox), "0.0");
    g_object_set_data(G_OBJECT(pref_gmenu->fpsTextBox), "parent", (gpointer) window);
    g_object_set_data(G_OBJECT(pref_gmenu->fpsTextBox), "rotation", (gpointer) rot_gmenu);
    g_object_set_data(G_OBJECT(pref_gmenu->fpsTextBox), "kemoview", (gpointer) kemoviewer_data);

    pref_gmenu->fpsButton = gtk_button_new_with_label("Start FPS test");
    g_object_set_data(G_OBJECT(window), "kemoview", (gpointer) kemoviewer_data);
    g_signal_connect(G_OBJECT(pref_gmenu->fpsButton), "clicked",
                     G_CALLBACK(rot_FPS_view_CB), (gpointer)pref_gmenu->fpsTextBox);

    pref_gmenu->FPStest_hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->FPStest_hbox), pref_gmenu->fpsButton,  FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->FPStest_hbox), gtk_label_new("Average FPS:"), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->FPStest_hbox), pref_gmenu->fpsTextBox, FALSE, FALSE, 0);

    
    pref_gmenu->Frame_BGsel = init_light_list_frame(kemoviewer_data,
                                                    pref_gmenu->lightparams_vws);
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
                     G_CALLBACK(AmbientChange_CB), (gpointer) kemoviewer_data);
	g_signal_connect(pref_gmenu->spin_diffuse, "value-changed",
                     G_CALLBACK(DiffuseChange_CB), (gpointer) kemoviewer_data);
	g_signal_connect(pref_gmenu->spin_specular, "value-changed",
                     G_CALLBACK(SpecularChange_CB), (gpointer) kemoviewer_data);
	g_signal_connect(pref_gmenu->spin_shineness, "value-changed",
                     G_CALLBACK(ShinenessChange_CB), (gpointer) kemoviewer_data);
	
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
    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), pref_gmenu->nthread_hbox, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), pref_gmenu->Frame_BGsel, TRUE, TRUE, 0);
	
	gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), pref_gmenu->pref_hbox[0], FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), pref_gmenu->pref_hbox[1], FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), pref_gmenu->pref_hbox[2], FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), pref_gmenu->pref_hbox[3], FALSE, FALSE, 0);

    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), pref_gmenu->nTubeCorner_hbox, FALSE, FALSE, 0);


    gtk_box_pack_start(GTK_BOX(pref_gmenu->pref_vbox), pref_gmenu->FPStest_hbox, FALSE, FALSE, 0);
    return;
}

GtkWidget * init_preference_frame(struct kemoviewer_type *kemoviewer_data,
                                  struct rotation_gtk_menu *rot_gmenu,
                                  struct preference_gtk_menu *pref_gmenu,
                                  GtkWidget *window){
    GtkWidget *frame_pref  = gtk_frame_new("Preferences");
    gtk_frame_set_shadow_type(GTK_FRAME(frame_pref), GTK_SHADOW_IN);
    init_preference_vbox(kemoviewer_data, rot_gmenu, pref_gmenu, window);
    gtk_container_add(GTK_CONTAINER(frame_pref), pref_gmenu->pref_vbox);
    return frame_pref;
}

GtkWidget * init_preference_expander(struct kemoviewer_type *kemoviewer_data,
                                     struct rotation_gtk_menu *rot_gmenu,
                                     struct preference_gtk_menu *pref_gmenu,
                                     GtkWidget *window){
    GtkWidget *expander_pref;
    init_preference_vbox(kemoviewer_data, rot_gmenu, pref_gmenu, window);
    expander_pref = wrap_into_scroll_expansion_gtk("Preferences", 160, 400,
                                                   window, pref_gmenu->pref_vbox);
    return expander_pref;
}
