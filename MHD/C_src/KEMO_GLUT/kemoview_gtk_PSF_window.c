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
    GtkWidget *window_main = GTK_WIDGET(user_data);
	struct psf_gtk_menu *psf_gmenu = (struct psf_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "psfmenu");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");

    int index_mode = gtk_selected_combobox_index(combobox_psfs);
    if(index_mode < 0){index_mode = 0;};
    kemoview_set_PSF_loaded_params(SET_CURRENT, index_mode, kemo_sgl);

    kemoview_set_each_PSF_field_param(FIELD_SEL_FLAG, IZERO, kemo_sgl);
    kemoview_set_each_PSF_field_param(COMPONENT_SEL_FLAG, IZERO, kemo_sgl);
    set_vector_plot_availablity(kemo_sgl, psf_gmenu);

    gtk_widget_destroy(psf_gmenu->psf_frame);
    psf_gmenu->psf_frame = set_psf_menu_box(kemo_sgl, kemo_gl, psf_gmenu, psf_gmenu->psfWin);
    gtk_container_add(GTK_CONTAINER(psf_gmenu->psfWin), psf_gmenu->psf_frame);
    gtk_widget_show_all(psf_gmenu->psfWin);
    gtk_widget_queue_draw(psf_gmenu->psfWin);
    draw_full(kemo_sgl);
	return;
};

static void close_psf_CB(GtkButton *button, gpointer user_data){
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(button), "parent"));
    struct psf_gtk_menu *psf_gmenu = (struct psf_gtk_menu *) g_object_get_data(G_OBJECT(button), "psfmenu");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(button), "kemoview");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(button), "kemoview_gl");

    int num_loaded = kemoview_close_PSF_view(kemo_sgl);
    set_GLFW_viewtype_mode(VIEW_3D);
    kemoview_set_viewtype(VIEW_3D, kemo_sgl);
	
    init_psf_window(kemo_sgl, kemo_gl, psf_gmenu);
    gtk_widget_queue_draw(psf_gmenu->psfWin);
    draw_full(kemo_sgl);
};

static void psf_field_select_CB(GtkComboBox *combobox_field, gpointer user_data)
{
    GtkWidget *window_main = GTK_WIDGET(user_data);
    struct psf_gtk_menu *psf_gmenu = (struct psf_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "psfmenu");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");

    int index_mode = gtk_selected_combobox_index(combobox_field);
    int num_fld = kemoview_get_each_PSF_field_param(kemo_sgl, NUM_FIELD_FLAG);
    if(index_mode >= num_fld || index_mode < 0){
        index_mode = 0;
    }

	kemoview_set_each_PSF_field_param(FIELD_SEL_FLAG, index_mode, kemo_sgl);
    kemoview_set_each_PSF_field_param(COMPONENT_SEL_FLAG, IZERO, kemo_sgl);

    gtk_widget_destroy(psf_gmenu->psf_frame);
    psf_gmenu->psf_frame = set_psf_menu_box(kemo_sgl, kemo_gl, psf_gmenu, psf_gmenu->psfWin);
    gtk_container_add(GTK_CONTAINER(psf_gmenu->psfWin), psf_gmenu->psf_frame);
    gtk_widget_show_all(psf_gmenu->psfWin);
    gtk_widget_queue_draw(psf_gmenu->psfWin);
    draw_full(kemo_sgl);
	return;
};

static void psf_component_select_CB(GtkComboBox *combobox_comp, gpointer user_data)
{
    GtkWidget *window_main = GTK_WIDGET(user_data);
    struct psf_gtk_menu *psf_gmenu = (struct psf_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "psfmenu");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(combobox_comp), "kemoview");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(combobox_comp), "kemoview_gl");

    int index_mode = gtk_selected_combobox_index(combobox_comp);
    int if_psf = kemoview_get_each_PSF_field_param(kemo_sgl, FIELD_SEL_FLAG);
    int ncomp = (int) kemoview_get_PSF_num_component(kemo_sgl, if_psf);
    if(index_mode >= ncomp || index_mode < 0){
        index_mode = 0;
    }
	
	kemoview_set_each_PSF_field_param(COMPONENT_SEL_FLAG, index_mode, kemo_sgl);
    
    gtk_widget_destroy(psf_gmenu->psf_frame);
    psf_gmenu->psf_frame = set_psf_menu_box(kemo_sgl, kemo_gl, psf_gmenu, psf_gmenu->psfWin);
    gtk_container_add(GTK_CONTAINER(psf_gmenu->psfWin), psf_gmenu->psf_frame);
    gtk_widget_show_all(psf_gmenu->psfWin);
    gtk_widget_queue_draw(psf_gmenu->psfWin);
    draw_full(kemo_sgl);
	return;
};

static void init_psf_draw_component_hbox(struct kemoviewer_type *kemo_sgl,
                                         struct kemoviewer_gl_type *kemo_gl,
                                         struct psf_gtk_menu *psf_gmenu, GtkWidget *window){
	char comp_name[1024];
	int if_psf = kemoview_get_each_PSF_field_param(kemo_sgl, FIELD_SEL_FLAG);
	int ncomp = (int) kemoview_get_PSF_num_component(kemo_sgl, if_psf);
	int icomp;
	
    psf_gmenu->comp_label_tree_view = create_fixed_label_w_index_tree();
	GtkTreeModel *model_comp = gtk_tree_view_get_model(GTK_TREE_VIEW(psf_gmenu->comp_label_tree_view));
	GtkTreeModel *child_model_comp = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_comp));
	
	int id_coord = kemoview_get_each_PSF_field_param(kemo_sgl, COORDINATE_FLAG);
	int index = 0;
	for(icomp=0;icomp<ncomp;icomp++){
		set_PSF_component_name(ncomp, id_coord, icomp, comp_name);
		index = append_ci_item_to_tree(index, comp_name, icomp, child_model_comp);
	};
	
	icomp = kemoview_get_each_PSF_field_param(kemo_sgl, COMPONENT_SEL_FLAG);
    psf_gmenu->renderer_comp = gtk_cell_renderer_text_new();
    psf_gmenu->combobox_comp = gtk_combo_box_new();
    gtk_combo_box_set_model(GTK_COMBO_BOX(psf_gmenu->combobox_comp), child_model_comp);
	gtk_combo_box_set_active(GTK_COMBO_BOX(psf_gmenu->combobox_comp), icomp);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(psf_gmenu->combobox_comp),
                               psf_gmenu->renderer_comp, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(psf_gmenu->combobox_comp),
                                   psf_gmenu->renderer_comp,
                                   "text", COLUMN_FIELD_NAME, NULL);

    g_object_set_data(G_OBJECT(window), "psfmenu", (gpointer) psf_gmenu);
    g_object_set_data(G_OBJECT(psf_gmenu->combobox_comp), "kemoview", (gpointer) kemo_sgl);
    g_object_set_data(G_OBJECT(psf_gmenu->combobox_comp), "kemoview_gl", (gpointer) kemo_gl);
	g_signal_connect(G_OBJECT(psf_gmenu->combobox_comp), "changed",
                     G_CALLBACK(psf_component_select_CB), (gpointer) window);
	
    psf_gmenu->hbox_comp = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(psf_gmenu->hbox_comp),
                       gtk_label_new("Component: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(psf_gmenu->hbox_comp),
                       psf_gmenu->combobox_comp, FALSE, FALSE, 0);
	return;
}

static void init_psf_draw_field_hbox(struct kemoviewer_type *kemo_sgl,
                                     struct kemoviewer_gl_type *kemo_gl,
                                     struct psf_gtk_menu *psf_gmenu,
                                     GtkWidget *window){
	struct kv_string *colorname = kemoview_alloc_kvstring();
	int num_field = kemoview_get_each_PSF_field_param(kemo_sgl, NUM_FIELD_FLAG);
	int if_psf =    kemoview_get_each_PSF_field_param(kemo_sgl, FIELD_SEL_FLAG);
	int ifld;
	
    psf_gmenu->field_label_tree_view = create_fixed_label_w_index_tree();
	GtkTreeModel *model_field =       gtk_tree_view_get_model(GTK_TREE_VIEW(psf_gmenu->field_label_tree_view));
	GtkTreeModel *child_model_field = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_field));
	
	int index = 0;
	for(ifld=0;ifld<num_field;ifld++){
		kemoview_get_PSF_field_name(kemo_sgl, colorname, ifld);
		index = append_ci_item_to_tree(index, colorname->string, ifld, child_model_field);
	};

    psf_gmenu->renderer_field = gtk_cell_renderer_text_new();
    psf_gmenu->combobox_field = gtk_combo_box_new_with_model(child_model_field);
	gtk_combo_box_set_active(GTK_COMBO_BOX(psf_gmenu->combobox_field), if_psf);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(psf_gmenu->combobox_field),
                               psf_gmenu->renderer_field, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(psf_gmenu->combobox_field),
                                   psf_gmenu->renderer_field,
                                   "text", COLUMN_FIELD_NAME, NULL);
    
    g_object_set_data(G_OBJECT(window), "psfmenu", (gpointer) psf_gmenu);
    g_object_set_data(G_OBJECT(window), "kemoview", (gpointer) kemo_sgl);
    g_object_set_data(G_OBJECT(window), "kemoview_gl", (gpointer) kemo_gl);
	g_signal_connect(G_OBJECT(psf_gmenu->combobox_field), "changed",
                     G_CALLBACK(psf_field_select_CB), (gpointer) window);
	
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

static void init_current_psf_set_hbox(struct kemoviewer_type *kemo_sgl,
                                      struct kemoviewer_gl_type *kemo_gl,
                                      struct psf_gtk_menu *psf_gmenu,
                                      GtkWidget *window){
	int index = 0;
    int ipsf, icou;;
	
	struct kv_string *stripped_filehead;
	char label_tmp[512];
	int id_current = kemoview_get_PSF_loaded_params(kemo_sgl, SET_CURRENT);
	int index_current = 0;

    psf_gmenu->num_psfs = count_loaded_psf(kemo_sgl);

    psf_gmenu->psf_label_tree_view = create_fixed_label_w_index_tree();
    GtkTreeModel *model_psfs = gtk_tree_view_get_model(GTK_TREE_VIEW(psf_gmenu->psf_label_tree_view));
    GtkTreeModel *child_model_psfs = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_psfs));
    index = 0;
    icou = 0;
    for (ipsf=0; ipsf< kemoview_get_PSF_loaded_params(kemo_sgl, MAX_LOADED); ipsf++){
        if(ipsf == id_current) {index_current = index;};
        if(kemoview_get_PSF_loaded_flag(kemo_sgl, ipsf) > 0) {
            kemoview_set_PSF_loaded_params(SET_CURRENT, ipsf, kemo_sgl);
            stripped_filehead = kemoview_alloc_kvstring();
            kemoview_get_PSF_file_prefix(kemo_sgl, stripped_filehead);
            sprintf(label_tmp, "%d: %s", ipsf, stripped_filehead->string);
            index = append_ci_item_to_tree(index, label_tmp,
                                           ipsf, child_model_psfs);
            kemoview_free_kvstring(stripped_filehead);
        };
        kemoview_set_PSF_loaded_params(SET_CURRENT, id_current, kemo_sgl);
    };

    psf_gmenu->renderer_psfs = gtk_cell_renderer_text_new();
    psf_gmenu->combobox_psfs = gtk_combo_box_new_with_model(child_model_psfs);
    gtk_combo_box_set_active(GTK_COMBO_BOX(psf_gmenu->combobox_psfs), index_current);
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(psf_gmenu->combobox_psfs),
                               psf_gmenu->renderer_psfs, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(psf_gmenu->combobox_psfs),
                                   psf_gmenu->renderer_psfs,
                                   "text", COLUMN_FIELD_NAME, NULL);
    printf("Make PSF selector end\n");

    g_object_set_data(G_OBJECT(window), "psfmenu", (gpointer) psf_gmenu);
    g_object_set_data(G_OBJECT(window), "kemoview", (gpointer) kemo_sgl);
    g_object_set_data(G_OBJECT(window), "kemoview_gl", (gpointer) kemo_gl);
    g_signal_connect(G_OBJECT(psf_gmenu->combobox_psfs), "changed",
                     G_CALLBACK(current_psf_select_CB), (gpointer) window);
    
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

GtkWidget * set_psf_menu_box(struct kemoviewer_type *kemo_sgl,
                             struct kemoviewer_gl_type *kemo_gl,
                             struct psf_gtk_menu *psf_gmenu,
                             GtkWidget *window){
    psf_gmenu->closeButton = gtk_button_new_with_label("Close Current PSF");
    
    g_object_set_data(G_OBJECT(psf_gmenu->closeButton), "parent", (gpointer) window);
    g_object_set_data(G_OBJECT(psf_gmenu->closeButton), "psfmenu", (gpointer) psf_gmenu);
    g_object_set_data(G_OBJECT(psf_gmenu->closeButton), "kemoview", (gpointer) kemo_sgl);
    g_object_set_data(G_OBJECT(psf_gmenu->closeButton), "kemoview_gl", (gpointer) kemo_gl);
    g_signal_connect(G_OBJECT(psf_gmenu->closeButton), "clicked",
                     G_CALLBACK(close_psf_CB), NULL);
    
    init_current_psf_set_hbox(kemo_sgl, kemo_gl, psf_gmenu, window);
    init_psf_draw_field_hbox(kemo_sgl, kemo_gl, psf_gmenu, window);
    
    init_psf_draw_component_hbox(kemo_sgl, kemo_gl, psf_gmenu, window);
    init_colormap_params_4_viewer(kemo_sgl, psf_gmenu->color_vws);
    
    init_psf_menu_hbox(kemo_sgl, kemo_gl, psf_gmenu, psf_gmenu->psfWin);
    
    
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

void init_psf_window(struct kemoviewer_type *kemo_sgl,
                     struct kemoviewer_gl_type *kemo_gl,
                     struct psf_gtk_menu *psf_gmenu){
    if(psf_gmenu->iflag_psfBox > 0){gtk_widget_destroy(psf_gmenu->psfWin);};
    psf_gmenu->iflag_psfBox = kemoview_get_PSF_loaded_params(kemo_sgl, NUM_LOADED);
    if(psf_gmenu->iflag_psfBox == 0){return;}

    psf_gmenu->psfWin = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(psf_gmenu->psfWin), "PSF");
    gtk_widget_set_size_request(psf_gmenu->psfWin, 150, -1);
    gtk_container_set_border_width(GTK_CONTAINER(psf_gmenu->psfWin), 5);
    
    psf_gmenu->psf_frame = set_psf_menu_box(kemo_sgl, kemo_gl, psf_gmenu, psf_gmenu->psfWin);
    gtk_container_add(GTK_CONTAINER(psf_gmenu->psfWin), psf_gmenu->psf_frame);
    gtk_widget_show_all(psf_gmenu->psfWin);
    return;
}

