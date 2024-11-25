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
	GtkWidget *window = GTK_WIDGET(user_data);
    struct colormap_view *color_vws
            = (struct colormap_view *) g_object_get_data(G_OBJECT(user_data), "colormap_view");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
	struct kv_string *filename = kemoview_save_file_panel(window);
	
	if(filename->string[0] != '\0'){
        kemoview_write_colormap_file(filename, color_vws->iflag_current_model,
                                     kemo_gl->kemoview_data);
    };
	kemoview_free_kvstring(filename);
    draw_full_gl(kemo_gl);
	return;
};

static void load_colormap_file_panel_CB(GtkButton *loadButton, gpointer user_data){
	GtkWidget *window = GTK_WIDGET(user_data);
    struct colormap_view *color_vws
            = (struct colormap_view *) g_object_get_data(G_OBJECT(user_data), "colormap_view");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
	struct kv_string *filename = kemoview_read_file_panel(window);
	
	if(filename->string[0] != '\0'){
        kemoview_read_colormap_file(filename, color_vws->iflag_current_model,
                                    kemo_gl->kemoview_data);
    };
	kemoview_free_kvstring(filename);
	
	gtk_widget_queue_draw(window);
    draw_full_gl(kemo_gl);
	return;
};


GtkWidget * init_gtk_psf_colormap_expander(struct kemoviewer_gl_type *kemo_gl,
                                           GtkWidget *window,
                                           struct colormap_view *color_vws){
    GtkWidget *expander_color;
    GtkWidget *saveButton = gtk_button_new_with_label("Save colormap...");
    
    GtkWidget *dummy_enty = gtk_entry_new();
    g_object_set_data(G_OBJECT(dummy_enty), "kemoview_gl",    (gpointer) kemo_gl);
    g_object_set_data(G_OBJECT(dummy_enty), "colormap_view",  (gpointer) color_vws);

	g_signal_connect(G_OBJECT(saveButton), "clicked",
				G_CALLBACK(save_colormap_file_panel_CB), G_OBJECT(dummy_enty));
	
	GtkWidget *loadButton = gtk_button_new_with_label("Load colormap...");
	g_signal_connect(G_OBJECT(loadButton), "clicked",
				G_CALLBACK(load_colormap_file_panel_CB), G_OBJECT(dummy_enty));
	
    GtkWidget *frame_box = init_kemoview_colormap_list_vbox(kemo_gl, color_vws);
    GtkWidget *color_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
    gtk_box_pack_start(GTK_BOX(color_box), frame_box, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(color_box), saveButton, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(color_box), loadButton, FALSE, FALSE, 0);

    expander_color = wrap_into_scroll_expansion_gtk("Color map editor", 420, 450, window, color_box);
	return expander_color;
}

struct psf_gtk_menu * alloc_psf_gtk_menu(){
	struct psf_gtk_menu *psf_gmenu = (struct psf_gtk_menu *) malloc(sizeof(struct psf_gtk_menu));
    if (psf_gmenu == NULL) {
        printf("malloc error for psf_gtk_menu\n");
        exit(0);
    }

	psf_gmenu->psf_isoline_menu = (struct psf_isoline_gtk_menu *) malloc(sizeof(struct psf_isoline_gtk_menu));
    if (psf_gmenu->psf_isoline_menu == NULL) {
        printf("malloc error for psf_gmenu->psf_isoline_menu\n");
        exit(0);
    }

	psf_gmenu->psf_surface_menu = (struct psf_surface_gtk_menu *) malloc(sizeof(struct psf_surface_gtk_menu));
    if (psf_gmenu->psf_surface_menu == NULL) {
        printf("malloc error for psf_gmenu->psf_surface_menu\n");
        exit(0);
    }

	psf_gmenu->psf_vector_menu =  (struct psf_vector_gtk_menu *) malloc(sizeof(struct psf_vector_gtk_menu));
    if (psf_gmenu->psf_vector_menu == NULL) {
        printf("malloc error for psf_gmenu->psf_vector_menu\n");
        exit(0);
    }

    psf_gmenu->color_vws = alloc_colormap_view();
	return psf_gmenu;
};

void dealloc_psf_gtk_menu(struct psf_gtk_menu *psf_gmenu){
    free(psf_gmenu->color_vws);
	free(psf_gmenu->psf_isoline_menu);
	free(psf_gmenu->psf_surface_menu);
	free(psf_gmenu->psf_vector_menu);
	free(psf_gmenu);
	return;
};

void set_vector_plot_availablity(int iflag_current_model,
                                 struct kemoviewer_gl_type *kemo_gl,
                                 struct psf_gtk_menu *psf_gmenu){
    int if_psf = kemoview_get_VIZ_field_param(kemo_gl->kemoview_data,
                                              iflag_current_model,
                                              FIELD_SEL_FLAG);
    int ncomp = (int) kemoview_get_VIZ_num_component(kemo_gl->kemoview_data,
                                                     iflag_current_model, if_psf);
    if(ncomp == 3){
        gtk_widget_set_sensitive(psf_gmenu->expander_vect, TRUE);
    } else {
        gtk_widget_set_sensitive(psf_gmenu->expander_vect, FALSE);
    };
    return;
};

void init_psf_menu_hbox(struct kemoviewer_gl_type *kemo_gl,
                        struct psf_gtk_menu *psf_gmenu){
    psf_gmenu->expander_iso = init_isoline_menu_expander(kemo_gl, psf_gmenu->psfWin,
                                                         psf_gmenu->psf_isoline_menu);
    psf_gmenu->expander_surf = init_gtk_psf_surface_menu_expander(kemo_gl, psf_gmenu->psfWin,
                                                                  psf_gmenu->psf_surface_menu);
    
    init_colormap_params_4_viewer(SURFACE_RENDERING, kemo_gl, psf_gmenu->color_vws);
    psf_gmenu->expander_color = init_gtk_psf_colormap_expander(kemo_gl, psf_gmenu->psfWin,
                                                               psf_gmenu->color_vws);
    set_gtk_surface_menu_values(kemo_gl, psf_gmenu->psf_surface_menu);
    set_gtk_isoline_menu_values(kemo_gl, psf_gmenu->psf_isoline_menu);

	
    psf_gmenu->expander_vect = make_gtk_psf_vector_menu(kemo_gl, psf_gmenu->psfWin,
                                                        psf_gmenu->psf_vector_menu);
    set_gtk_psf_vector_menu(kemo_gl, psf_gmenu->psf_vector_menu);
    set_vector_plot_availablity(SURFACE_RENDERING, kemo_gl, psf_gmenu);
    return;
}
