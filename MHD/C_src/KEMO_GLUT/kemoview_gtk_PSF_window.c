/*
 *  kemoview_gtk_PSF_window.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_PSF_window.h"

static void current_psf_select_CB(GtkComboBox *combobox_psfs, gpointer user_data)
{
    GtkWidget *itemTEvo = GTK_WIDGET(user_data);
	struct psf_gtk_menu *psf_gmenu = (struct psf_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "psfmenu");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
    kemoview_psf_select_CB(combobox_psfs, SURFACE_RENDERING, kemo_gl);
    set_vector_plot_availablity(SURFACE_RENDERING, kemo_gl, psf_gmenu);
    replace_psf_menu_frame(kemo_gl, psf_gmenu, itemTEvo);
    draw_full_gl(kemo_gl);
	return;
};

static void close_psf_CB(GtkButton *button, gpointer user_data){
    GtkWidget *itemTEvo = GTK_WIDGET(user_data);
    GtkWidget *main_window = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parents"));
    struct psf_gtk_menu *psf_gmenu = (struct psf_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "psfmenu");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");

    int num_loaded = kemoview_close_PSF_view(kemo_gl->kemoview_data);
    set_GLFW_viewtype_mode(VIEW_3D);
    kemoview_set_viewtype(VIEW_3D, kemo_gl->kemoview_data);
	
    init_psf_window(kemo_gl, psf_gmenu, main_window, itemTEvo);
    activate_evolution_menu(kemo_gl->kemoview_data, itemTEvo);
    gtk_widget_queue_draw(psf_gmenu->psfWin);
    draw_full_gl(kemo_gl);
};

static void psf_field_select_CB(GtkComboBox *combobox_field, gpointer user_data)
{
    GtkWidget *itemTEvo = GTK_WIDGET(user_data);
    struct psf_gtk_menu *psf_gmenu = (struct psf_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "psfmenu");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
    kemoview_field_select_CB(combobox_field, SURFACE_RENDERING, kemo_gl);
    replace_psf_menu_frame(kemo_gl, psf_gmenu, itemTEvo);
    draw_full_gl(kemo_gl);
	return;
};

static void psf_component_select_CB(GtkComboBox *combobox_comp, gpointer user_data)
{
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
    kemoview_component_select_CB(combobox_comp, SURFACE_RENDERING, kemo_gl);
    draw_full_gl(kemo_gl);
	return;
};

static void init_psf_draw_component_hbox(struct kemoviewer_gl_type *kemo_gl,
                                         struct psf_gtk_menu *psf_gmenu, GtkWidget *itemTEvo){
    psf_gmenu->combobox_comp = draw_viz_component_gtk_box(kemo_gl, SURFACE_RENDERING);
	g_signal_connect(G_OBJECT(psf_gmenu->combobox_comp), "changed",
                     G_CALLBACK(psf_component_select_CB), (gpointer) itemTEvo);
	
    psf_gmenu->hbox_comp = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(psf_gmenu->hbox_comp),
                       gtk_label_new("Component: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(psf_gmenu->hbox_comp),
                       psf_gmenu->combobox_comp, FALSE, FALSE, 0);
	return;
}

static void init_psf_draw_field_hbox(struct kemoviewer_gl_type *kemo_gl,
                                     struct psf_gtk_menu *psf_gmenu,
                                     GtkWidget *itemTEvo){
    psf_gmenu->combobox_field = draw_viz_field_gtk_box(kemo_gl, SURFACE_RENDERING);
	g_signal_connect(G_OBJECT(psf_gmenu->combobox_field), "changed",
                     G_CALLBACK(psf_field_select_CB), (gpointer) itemTEvo);
	
    psf_gmenu->hbox_field = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(psf_gmenu->hbox_field), gtk_label_new("Field: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(psf_gmenu->hbox_field), psf_gmenu->combobox_field,
                       FALSE, FALSE, 0);
	return;
}


static int count_loaded_psf(struct kemoviewer_type *kemo_sgl){
    int ipsf;
    int num_psfs = 0;
    for (ipsf=0; ipsf< kemoview_get_PSF_loaded_params(kemo_sgl, MAX_LOADED); ipsf++){
        if(kemoview_get_PSF_loaded_flag(kemo_sgl, ipsf) > 0) {
            num_psfs = num_psfs + 1;
        };
    };
    return num_psfs;
}

static void init_current_psf_set_hbox(struct kemoviewer_gl_type *kemo_gl,
                                      struct psf_gtk_menu *psf_gmenu,
                                      GtkWidget *itemTEvo){
	int id_current_psf = kemoview_get_PSF_loaded_params(kemo_gl->kemoview_data,
                                                        SET_CURRENT);
	int index = 0;
    psf_gmenu->num_psfs =      count_loaded_psf(kemo_gl->kemoview_data);
    psf_gmenu->combobox_psfs = draw_current_psf_set_hbox(id_current_psf, kemo_gl, &index);
    g_signal_connect(G_OBJECT(psf_gmenu->combobox_psfs), "changed",
                     G_CALLBACK(current_psf_select_CB), (gpointer) itemTEvo);
    
    psf_gmenu->hbox_psfs = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(psf_gmenu->hbox_psfs), gtk_label_new("Current PSF: "),
                       FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(psf_gmenu->hbox_psfs), psf_gmenu->combobox_psfs,
                       FALSE, FALSE, 0);

    if(index <= 1){
        gtk_widget_set_sensitive(psf_gmenu->combobox_psfs, FALSE);
    }else{
        gtk_widget_set_sensitive(psf_gmenu->combobox_psfs, TRUE);
    }
    return;
}

GtkWidget * init_psf_menu_frame(struct kemoviewer_gl_type *kemo_gl,
                                struct psf_gtk_menu *psf_gmenu,
                                GtkWidget *itemTEvo){
    psf_gmenu->closeButton = gtk_button_new_with_label("Close Current PSF");
    
    g_signal_connect(G_OBJECT(psf_gmenu->closeButton), "clicked",
                     G_CALLBACK(close_psf_CB), itemTEvo);
    
    init_current_psf_set_hbox(kemo_gl, psf_gmenu, itemTEvo);
    init_psf_draw_field_hbox(kemo_gl, psf_gmenu, itemTEvo);
    
    init_psf_draw_component_hbox(kemo_gl, psf_gmenu, itemTEvo);
    init_psf_menu_hbox(kemo_gl, psf_gmenu);
    
    GtkWidget *psf_vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->closeButton, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->hbox_psfs, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->hbox_field, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->hbox_comp, TRUE, TRUE, 0);

    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->expander_iso, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->expander_surf, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->expander_color, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->expander_vect, FALSE, TRUE, 0);
    return wrap_into_frame_gtk("Surfaces", psf_vbox);;
}

void replace_psf_menu_frame(struct kemoviewer_gl_type *kemo_gl,
                            struct psf_gtk_menu *psf_gmenu,
                            GtkWidget *itemTEvo){
    gtk_widget_destroy(psf_gmenu->psf_frame);
    psf_gmenu->psf_frame = init_psf_menu_frame(kemo_gl, psf_gmenu, itemTEvo);
    gtk_container_add(GTK_CONTAINER(psf_gmenu->psfWin), psf_gmenu->psf_frame);
    gtk_widget_show_all(psf_gmenu->psfWin);
    gtk_widget_queue_draw(psf_gmenu->psfWin);
    return;
}

void init_psf_window(struct kemoviewer_gl_type *kemo_gl,
                     struct psf_gtk_menu *psf_gmenu,
                     GtkWidget *main_window, GtkWidget *itemTEvo){
    if(psf_gmenu->iflag_psfBox > 0){gtk_widget_destroy(psf_gmenu->psfWin);};
    psf_gmenu->iflag_psfBox = kemoview_get_PSF_loaded_params(kemo_gl->kemoview_data,
                                                             NUM_LOADED);
    if(psf_gmenu->iflag_psfBox == 0){return;}

    gint win_upperleft[2], size_xy[2];
    gtk_window_get_position(GTK_WINDOW(main_window), &win_upperleft[0], &win_upperleft[1]);
    gtk_window_get_size(GTK_WINDOW(main_window), &size_xy[0], &size_xy[1]);

    psf_gmenu->psfWin = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_move(GTK_WINDOW(psf_gmenu->psfWin),
                    win_upperleft[0], (win_upperleft[1]+size_xy[1]+56));
    gtk_window_set_title(GTK_WINDOW(psf_gmenu->psfWin), "PSF");
    gtk_widget_set_size_request(psf_gmenu->psfWin, 150, -1);
    gtk_container_set_border_width(GTK_CONTAINER(psf_gmenu->psfWin), 5);
    
    g_object_set_data(G_OBJECT(itemTEvo), "psfmenu", (gpointer) psf_gmenu);
    g_object_set_data(G_OBJECT(itemTEvo), "kemoview_gl", (gpointer) kemo_gl);
    g_object_set_data(G_OBJECT(itemTEvo), "parents", (gpointer) main_window);
    psf_gmenu->psf_frame = init_psf_menu_frame(kemo_gl, psf_gmenu, itemTEvo);
    gtk_container_add(GTK_CONTAINER(psf_gmenu->psfWin), psf_gmenu->psf_frame);
    gtk_widget_show_all(psf_gmenu->psfWin);
    return;
}

