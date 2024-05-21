/*
 *  kemoview_gtk_main_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_main_menu.h"


struct main_buttons * init_main_buttons(struct kemoviewer_type *kemoviewer_data){
	struct main_buttons *mbot = (struct main_buttons *) malloc(sizeof(struct main_buttons));
    if (mbot == NULL) {
        printf("malloc error for main_buttons\n");
        exit(0);
    }

    mbot->psf_gmenu = alloc_psf_gtk_menu();
    mbot->fline_menu = (struct fieldline_gtk_menu *) malloc(sizeof(struct fieldline_gtk_menu));
    mbot->mesh_vws = (struct kemoview_mesh_view *) malloc(sizeof(struct kemoview_mesh_view));
    mbot->evo_gmenu = init_evoluaiton_menu_box(kemoviewer_data);

    mbot->view_menu = (struct view_widgets *) malloc(sizeof(struct view_widgets));

	mbot->rot_gmenu = init_rotation_menu_box();
    mbot->quilt_gmenu = init_quilt_menu_box();
    mbot->lightparams_vws = init_light_views_4_viewer(kemoviewer_data->kemo_buffers->kemo_lights);
	return mbot;
};

void dealloc_main_buttons(struct main_buttons *mbot){
    dealloc_psf_gtk_menu(mbot->psf_gmenu);
    free(mbot->fline_menu);
    free(mbot->mesh_vws);
    free(mbot->evo_gmenu);

    dealloc_light_views_4_viewer(mbot->lightparams_vws);
	
	free(mbot->rot_gmenu);
	free(mbot->view_menu);
	
	free(mbot);
	return;
};

void open_kemoviewer_file_glfw(struct kemoviewer_type *kemo_sgl,
                               struct kemoviewer_gl_type *kemo_gl,
                               struct kv_string *filename,
                               struct main_buttons *mbot,
                               GtkWidget *window_main){
    struct kv_string *file_prefix = kemoview_alloc_kvstring();
    struct kv_string *stripped_ext = kemoview_alloc_kvstring();
	int iflag_datatype = kemoview_set_data_format_flag(filename, file_prefix, stripped_ext);
	
	printf("file name: %s\n", filename->string);
	printf("file_prefix %s\n", file_prefix->string);
	printf("stripped_ext %s\n", stripped_ext->string);
    kemoview_free_kvstring(stripped_ext);
    kemoview_free_kvstring(file_prefix);
	
	iflag_datatype = kemoview_open_data(filename, kemo_sgl);
    kemoview_free_kvstring(filename);
	
    init_psf_window(kemo_sgl, kemo_gl, mbot->psf_gmenu, window_main);
    init_fline_window(kemo_sgl, mbot->fline_menu, window_main);
    init_mesh_window(kemo_sgl, mbot->mesh_vws, mbot->meshWin);

    activate_evolution_menu(kemo_sgl, mbot->itemTEvo);
    gtk_widget_show_all(mbot->menuHbox);
	gtk_widget_queue_draw(window_main);
    draw_full(kemo_sgl);
	return;
};

void make_gtk_main_menu_box(struct main_buttons *mbot,
                            GtkWidget *takobox, GtkWidget *window_main,
                            struct kemoviewer_type *kemo_sgl,
                            struct kemoviewer_gl_type *kemo_gl){
	GtkWidget *hbox_viewtype = make_gtk_viewmode_menu_box(mbot->view_menu, kemo_sgl);
    GtkWidget *hbox_axis = make_axis_menu_box(kemo_sgl, window_main);
    GtkWidget *expander_rot = init_rotation_menu_expander(kemo_sgl,
                                                          mbot->rot_gmenu,
                                                          window_main);
//    GtkWidget *expander_evo = init_evolution_menu_expander(kemo_sgl, mbot->evo_gmenu, window_main);
    mbot->expander_view = init_viewmatrix_menu_expander(kemo_sgl,
                                                        mbot->view_menu,
                                                        window_main);
    mbot->expander_quilt = init_quilt_menu_expander(kemo_sgl,
                                                    mbot->quilt_gmenu,
                                                    mbot->view_menu, window_main);

	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), hbox_viewtype, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), hbox_axis, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), expander_rot, FALSE, FALSE, 0);
//    gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), expander_evo, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), mbot->expander_quilt, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), mbot->expander_view, FALSE, FALSE, 0);
    return;
}

