/*
 *  kemoview_gtk_main_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_main_menu.h"
#include "view_modifier_glfw.h"

static struct updatable_widgets * init_updatable_widgets(struct kemoviewer_type *kemoviewer_data){
    struct updatable_widgets *updatable = (struct updatable_widgets *) malloc(sizeof(struct updatable_widgets));
    if (updatable == NULL) {
        printf("malloc error for updatable_widgets\n");
        exit(0);
    }

    updatable->psf_gmenu = alloc_psf_gtk_menu();
    updatable->fline_menu = (struct fieldline_gtk_menu *) malloc(sizeof(struct fieldline_gtk_menu));
    updatable->mesh_vws = (struct kemoview_mesh_view *) malloc(sizeof(struct kemoview_mesh_view));
    updatable->evo_gmenu = init_evoluaiton_menu_box(kemoviewer_data);
    return updatable;
}
static void dealloc_updatable_widgets(struct updatable_widgets *updatable){
    dealloc_psf_gtk_menu(updatable->psf_gmenu);
    free(updatable->fline_menu);
    free(updatable->mesh_vws);
    free(updatable->evo_gmenu);
    free(updatable);
    return;
};


struct main_buttons * init_main_buttons(struct kemoviewer_type *kemoviewer_data){
	struct main_buttons *mbot = (struct main_buttons *) malloc(sizeof(struct main_buttons));
    if (mbot == NULL) {
        printf("malloc error for main_buttons\n");
        exit(0);
    }

    mbot->updatable = init_updatable_widgets(kemoviewer_data);
	mbot->view_menu = (struct view_widgets *) malloc(sizeof(struct view_widgets));

	mbot->rot_gmenu = init_rotation_menu_box();
    mbot->quilt_gmenu = init_quilt_menu_box();
	mbot->pref_gmenu = init_preference_gtk_menu(kemoviewer_data);
	return mbot;
};

void dealloc_main_buttons(struct main_buttons *mbot){
    dealloc_updatable_widgets(mbot->updatable);
	dealloc_preference_gtk_menu(mbot->pref_gmenu);
	
	free(mbot->rot_gmenu);
	free(mbot->view_menu);
	
	free(mbot);
	return;
};


void update_kemoview_menu(struct kemoviewer_type *kemo_sgl,
                          struct updatable_widgets *updatable,
                          GtkWidget *menuHbox, GtkWidget *window){
    int iflag_draw_m = kemoview_get_draw_mesh_flag(kemo_sgl);
    int iflag_draw_f = kemoview_get_fline_parameters(kemo_sgl, DRAW_SWITCH);
    int nload_psf = kemoview_get_PSF_loaded_params(kemo_sgl, NUM_LOADED);

    if(updatable->iflag_psfBox > 0){
        gtk_box_pack_start(GTK_BOX(menuHbox), updatable->psfBox, FALSE, FALSE, 0);
    };
    if(updatable->iflag_flineBox > 0){
        gtk_box_pack_start(GTK_BOX(menuHbox), updatable->flineBox, FALSE, FALSE, 0);
    };
    if(updatable->iflag_meshBox > 0){
        gtk_box_pack_start(GTK_BOX(menuHbox), updatable->meshBox, FALSE, FALSE, 0);
    };
	gtk_window_resize(GTK_WINDOW(window), 240, 200);
    gtk_widget_show_all(menuHbox);
	return;
};

static void init_psf_menu(struct kemoviewer_type *kemo_sgl,
                          struct kemoviewer_gl_type *kemo_gl,
                          struct updatable_widgets *updatable,
                          GtkWidget *window){
    if(kemoview_get_PSF_loaded_params(kemo_sgl, NUM_LOADED) == 0) return;
    if(updatable->iflag_psfBox == 0){
        updatable->psfBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
        set_psf_menu_box(kemo_sgl, kemo_gl, updatable, window);
        pack_psf_menu_frame(updatable->psf_gmenu);
        gtk_box_pack_start(GTK_BOX(updatable->psfBox), updatable->psf_gmenu->psf_frame,
                           FALSE, FALSE, 0);
        updatable->iflag_psfBox = 1;
    }else{
        update_current_psf_set_hbox(kemo_sgl, updatable->psf_gmenu);
        update_psf_draw_field_hbox(kemo_sgl, updatable->psf_gmenu);
        update_by_psf_field(kemo_sgl, updatable->psf_gmenu);
    };
    return;
}

static void init_fline_menu(struct kemoviewer_type *kemo_sgl,
                            struct updatable_widgets *updatable,
                            GtkWidget *window){
    if(kemoview_get_fline_parameters(kemo_sgl, DRAW_SWITCH) == 0) return;
    if(updatable->iflag_flineBox == 0){
        updatable->flineBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
        set_fieldline_menu_box(kemo_sgl, updatable->fline_menu, window);
        GtkWidget *frame = pack_fieldline_menu_frame(updatable->fline_menu);
        gtk_box_pack_start(GTK_BOX(updatable->flineBox), frame, FALSE, FALSE, 0);
        updatable->iflag_flineBox = 1;
    }else{
        update_fieldline_menu_hbox(kemo_sgl, updatable->fline_menu);
    };
    return;
}

static void init_mesh_menu(struct kemoviewer_type *kemo_sgl,
                           struct updatable_widgets *updatable, 
                           GtkWidget *window){
    if(updatable->iflag_meshBox > 0) return;
    if(kemoview_get_draw_mesh_flag(kemo_sgl) == 0) return;

    updatable->meshBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    set_mesh_menu_box(kemo_sgl, updatable, window);
    GtkWidget *frame_mesh = pack_gtk_mesh_menu(updatable->mesh_vws, window);
    gtk_box_pack_start(GTK_BOX(updatable->meshBox), frame_mesh, FALSE, FALSE, 0);
    updatable->iflag_meshBox = 1;
    return;
}

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
	
    init_psf_menu(kemo_sgl, kemo_gl, mbot->updatable, window_main);
    init_fline_menu(kemo_sgl, mbot->updatable, window_main);
    init_mesh_menu(kemo_sgl, mbot->updatable, window_main);
    activate_evolution_menu(kemo_sgl, mbot->updatable->expander_evo);
    
    update_kemoview_menu(kemo_sgl, mbot->updatable, mbot->menuHbox,
                         window_main);
    
	gtk_widget_queue_draw(window_main);
    draw_full(kemo_sgl);
	return;
};

static void set_viewtype_CB(GtkComboBox *combobox_viewtype, gpointer user_data)
{
    struct view_widgets *view_menu = (struct view_widgets *) user_data;
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(combobox_viewtype), "kemoview");

	int index_mode = gtk_selected_combobox_index(combobox_viewtype);
	
    set_GLFW_viewtype_mode(index_mode);
    kemoview_set_viewtype(index_mode, kemo_sgl);
    draw_full(kemo_sgl);
    
    if(kemoview_get_view_type_flag(kemo_sgl) == VIEW_STEREO){
        gtk_widget_set_sensitive(view_menu->Frame_stereo, TRUE);
    }else{
        gtk_widget_set_sensitive(view_menu->Frame_stereo, FALSE);
    };
	return;
};

static void set_image_fileformat_CB(GtkComboBox *combobox_filefmt, gpointer user_data)
{
	struct main_buttons *mbot = (struct main_buttons *) g_object_get_data(G_OBJECT(user_data), "buttons");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(combobox_filefmt), "kemoview");

	mbot->id_iamge_format = gtk_selected_combobox_index(combobox_filefmt);
	draw_full(kemo_sgl);
	return;
};

static void image_save_CB(GtkButton *button, gpointer user_data){
	struct main_buttons *mbot =        (struct main_buttons *)    g_object_get_data(G_OBJECT(user_data), "buttons");
    struct kemoviewer_type *kemo_sgl = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");
	int iflag_set = kemoview_gtk_save_file_select(button, user_data);
	int id_imagefmt_by_input;
    int i_quilt;
    
	if(iflag_set == IZERO) return;
	
    int iflag_quilt = kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_MODE);
    int npix_x = kemoview_get_view_integer(kemo_sgl, ISET_PIXEL_X);
    int npix_y = kemoview_get_view_integer(kemo_sgl, ISET_PIXEL_Y);
    unsigned char *image = kemoview_alloc_RGB_buffer_to_bmp(npix_x, npix_y);

    GtkEntry *entry = GTK_ENTRY(user_data);
	struct kv_string *filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
    struct kv_string *stripped_ext = kemoview_alloc_kvstring();
	struct kv_string *file_prefix = kemoview_alloc_kvstring();
	
	kemoview_get_ext_from_file_name(filename, file_prefix, stripped_ext);
	id_imagefmt_by_input = kemoview_set_image_file_format_id(stripped_ext);
	if(id_imagefmt_by_input < 0) {
		id_imagefmt_by_input = mbot->id_iamge_format;
		kemoview_free_kvstring(file_prefix);
		file_prefix = kemoview_init_kvstring_by_string(filename->string);
	};
	if(id_imagefmt_by_input == 0) return;
	kemoview_free_kvstring(filename);
	kemoview_free_kvstring(stripped_ext);
	
	printf("header: %s\n", file_prefix->string);
    if(iflag_quilt == 0){
        kemoview_get_gl_buffer_to_bmp(npix_x, npix_y, image);
        kemoview_write_window_to_file(id_imagefmt_by_input, file_prefix,
                                      npix_x, npix_y, image);
    } else {
        int nimg_column = kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_COLUMN);
        int nimg_raw =    kemoview_get_quilt_nums(kemo_sgl, ISET_QUILT_RAW);
        unsigned char *quilt_image = kemoview_alloc_RGB_buffer_to_bmp((nimg_column * npix_x),
                                                                      (nimg_raw * npix_y));
        for(i_quilt=0;i_quilt<(nimg_column*nimg_raw);i_quilt++){
            draw_quilt(i_quilt, kemo_sgl);
            kemoview_get_gl_buffer_to_bmp(npix_x, npix_y, image);
            kemoview_add_quilt_img(i_quilt, kemo_sgl, image, quilt_image);
        };
        kemoview_write_window_to_file(id_imagefmt_by_input, file_prefix,
                                      (nimg_column * npix_x),
                                      (nimg_raw * npix_y), quilt_image);
        free(quilt_image);
        printf("quilt! %d x %d\n", nimg_column, nimg_raw);
        draw_full(kemo_sgl);
    }
    free(image);
    kemoview_free_kvstring(file_prefix);
	
	return;
};

static void current_psf_select_CB(GtkComboBox *combobox_psfs, gpointer user_data)
{
    GtkWidget *window_main = GTK_WIDGET(user_data);
	struct psf_gtk_menu *psf_gmenu = (struct psf_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "psfmenu");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");

    int index_mode = gtk_selected_combobox_index(combobox_psfs);
    if(index_mode < 0){index_mode = 0;};
    kemoview_set_PSF_loaded_params(SET_CURRENT, index_mode, kemo_sgl);

    update_psf_draw_field_hbox(kemo_sgl, psf_gmenu);
    update_by_psf_field(kemo_sgl, psf_gmenu);

	gtk_widget_queue_draw(window_main);
    draw_full(kemo_sgl);
	return;
};

static void close_psf_CB(GtkButton *button, gpointer user_data){
    GtkWidget *expander_evo = GTK_WIDGET(user_data);
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
    GtkWidget *psfBox = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "psfbox"));
    struct psf_gtk_menu *psf_gmenu = (struct psf_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "psfmenu");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");

    int num_loaded = kemoview_close_PSF_view(kemo_sgl);
    set_GLFW_viewtype_mode(VIEW_3D);
    kemoview_set_viewtype(VIEW_3D, kemo_sgl);
	
    if(num_loaded > 0){
        update_current_psf_set_hbox(kemo_sgl, psf_gmenu);
        update_psf_draw_field_hbox(kemo_sgl, psf_gmenu);
        update_by_psf_field(kemo_sgl, psf_gmenu);
    }else{
//        gtk_widget_set_sensitive(psfBox, FALSE);
        gtk_widget_hide(psfBox);
    }
    
    activate_evolution_menu(kemo_sgl, expander_evo);
	gtk_widget_queue_draw(window_main);
    draw_full(kemo_sgl);
};

static void close_fline_CB(GtkButton *button, gpointer user_data){
    GtkWidget *window_main = GTK_WIDGET(user_data);
    struct fieldline_gtk_menu *fline_menu = (struct fieldline_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "flinemenu");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");

	kemoview_close_fieldline_view(kemo_sgl);
	
    update_fieldline_menu_hbox(kemo_sgl, fline_menu);

	gtk_widget_queue_draw(window_main);
    draw_full(kemo_sgl);
};

static void close_mesh_CB(GtkButton *button, gpointer user_data){
    GtkWidget *window_main = GTK_WIDGET(user_data);
    struct updatable_widgets *updatable = (struct updatable_widgets *) g_object_get_data(G_OBJECT(user_data), "updates");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");

	kemoview_close_mesh_view(kemo_sgl);
	dealloc_mesh_views_4_viewer(updatable->mesh_vws);
	
    gtk_widget_hide(updatable->meshBox);
	gtk_widget_queue_draw(window_main);
    draw_full(kemo_sgl);
	return;
};

static void update_by_psf_component(struct kemoviewer_type *kemo_sgl,
                                    struct psf_gtk_menu *psf_gmenu){
    set_gtk_surface_menu_values(kemo_sgl, psf_gmenu->psf_surface_menu);
    set_gtk_isoline_menu_values(kemo_sgl, psf_gmenu->psf_isoline_menu);
    update_colormap_params_4_viewer(kemo_sgl, psf_gmenu->color_vws);
    update_kemoview_cmap_list_box(psf_gmenu->color_vws);
    return;
}

void update_by_psf_field(struct kemoviewer_type *kemo_sgl,
                         struct psf_gtk_menu *psf_gmenu){
    update_psf_draw_component_hbox(kemo_sgl, psf_gmenu);
    update_by_psf_component(kemo_sgl, psf_gmenu);
    set_vector_plot_availablity(kemo_sgl, psf_gmenu);
    return;
}


static void psf_field_select_CB(GtkComboBox *combobox_field, gpointer user_data)
{
    GtkWidget *window_main = GTK_WIDGET(user_data);
    struct psf_gtk_menu *psf_gmenu = (struct psf_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "psfmenu");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(combobox_field), "kemoview");

    int index_mode = gtk_selected_combobox_index(combobox_field);
    int num_fld = kemoview_get_each_PSF_field_param(kemo_sgl, NUM_FIELD_FLAG);
    if(index_mode >= num_fld || index_mode < 0){
        index_mode = 0;
    }

	kemoview_set_each_PSF_field_param(FIELD_SEL_FLAG, index_mode, kemo_sgl);
    kemoview_set_each_PSF_field_param(COMPONENT_SEL_FLAG, IZERO, kemo_sgl);

    update_by_psf_field(kemo_sgl, psf_gmenu);

    gtk_widget_queue_draw(window_main);
    draw_full(kemo_sgl);
	return;
};

static void psf_component_select_CB(GtkComboBox *combobox_comp, gpointer user_data)
{
    GtkWidget *window_main = GTK_WIDGET(user_data);
    struct psf_gtk_menu *psf_gmenu = (struct psf_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "psfmenu");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(combobox_comp), "kemoview");

    int index_mode = gtk_selected_combobox_index(combobox_comp);
    int if_psf = kemoview_get_each_PSF_field_param(kemo_sgl, FIELD_SEL_FLAG);
    if(index_mode >= kemoview_get_PSF_num_component(kemo_sgl, if_psf) || index_mode < 0){
        index_mode = 0;
    }
	
	kemoview_set_each_PSF_field_param(COMPONENT_SEL_FLAG, index_mode, kemo_sgl);
    update_by_psf_component(kemo_sgl, psf_gmenu);
	gtk_widget_queue_draw(window_main);
    draw_full(kemo_sgl);
	return;
};


static void open_file_CB(GtkButton *button, gpointer user_data){
    struct kv_string *filename;
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");

	int iflag_set = kemoview_gtk_read_file_select(button, user_data);
	if(iflag_set == IZERO) return;
	GtkEntry *entry = GTK_ENTRY(user_data);
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct main_buttons *mbot = (struct main_buttons *) g_object_get_data(G_OBJECT(user_data), "buttons");
	filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
	
	open_kemoviewer_file_glfw(kemo_sgl, kemo_gl, filename, mbot, window_main);
	return;
};

int count_loaded_psf(struct kemoviewer_type *kemo_sgl){
    int ipsf;
    int num_psfs = 0;
    for (ipsf=0; ipsf< kemoview_get_PSF_loaded_params(kemo_sgl, MAX_LOADED); ipsf++){
        if(kemoview_get_PSF_loaded_flag(kemo_sgl, ipsf) > 0) {
            num_psfs = num_psfs + 1;
        };
    };
    return num_psfs;
}

void update_current_psf_set_hbox(struct kemoviewer_type *kemo_sgl,
                                 struct psf_gtk_menu *psf_gmenu){
    char label_tmp[512];
    int ipsf, index;
    int index_current = 0;
    int id_current = kemoview_get_PSF_loaded_params(kemo_sgl, SET_CURRENT);
    struct kv_string *stripped_filehead;

    psf_gmenu->num_psfs = count_loaded_psf(kemo_sgl);

    clear_ci_tree_view(GTK_TREE_VIEW(psf_gmenu->psf_label_tree_view));
    GtkTreeModel *model_comp = gtk_tree_view_get_model(GTK_TREE_VIEW(psf_gmenu->psf_label_tree_view));
    GtkTreeModel *child_model_psfs = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_comp));

    index = 0;
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
    gtk_combo_box_set_active(GTK_COMBO_BOX(psf_gmenu->combobox_field), id_current);
    
    if(psf_gmenu->num_psfs > 1){
        gtk_widget_set_sensitive(psf_gmenu->combobox_field, TRUE);
    }else{
        gtk_widget_set_sensitive(psf_gmenu->combobox_field, FALSE);
    };
}

static void init_current_psf_set_hbox(struct kemoviewer_type *kemo_sgl,
                                      struct psf_gtk_menu *psf_gmenu,
                                      GtkWidget *window){
	int index = 0;
	int ipsf;
	
	struct kv_string *stripped_filehead;
	char label_tmp[512];
	int id_current = kemoview_get_PSF_loaded_params(kemo_sgl, SET_CURRENT);
	int index_current = 0;

    psf_gmenu->num_psfs = count_loaded_psf(kemo_sgl);

    psf_gmenu->psf_label_tree_view = create_fixed_label_w_index_tree();
    GtkTreeModel *model_psfs = gtk_tree_view_get_model(GTK_TREE_VIEW(psf_gmenu->psf_label_tree_view));
    GtkTreeModel *child_model_psfs = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_psfs));
    index = 0;
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
    
    g_object_set_data(G_OBJECT(window), "psfmenu", (gpointer) psf_gmenu);
    g_object_set_data(G_OBJECT(window), "kemoview", (gpointer) kemo_sgl);
    g_signal_connect(G_OBJECT(psf_gmenu->combobox_psfs), "changed",
                     G_CALLBACK(current_psf_select_CB), (gpointer) window);
    
    psf_gmenu->hbox_psfs = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(psf_gmenu->hbox_psfs), gtk_label_new("Current PSF: "),
                       FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(psf_gmenu->hbox_psfs), psf_gmenu->combobox_psfs,
                       FALSE, FALSE, 0);

    gtk_widget_set_sensitive(psf_gmenu->combobox_psfs, FALSE);
    return;
}

void update_psf_draw_field_hbox(struct kemoviewer_type *kemo_sgl,
                                struct psf_gtk_menu *psf_gmenu){
    struct kv_string *colorname = kemoview_alloc_kvstring();
    int num_field = kemoview_get_each_PSF_field_param(kemo_sgl, NUM_FIELD_FLAG);
    int ifld;

    kemoview_set_each_PSF_field_param(FIELD_SEL_FLAG, IZERO, kemo_sgl);
    kemoview_set_each_PSF_field_param(COMPONENT_SEL_FLAG, IZERO, kemo_sgl);
    clear_ci_tree_view(GTK_TREE_VIEW(psf_gmenu->field_label_tree_view));
    GtkTreeModel *model_comp = gtk_tree_view_get_model(GTK_TREE_VIEW(psf_gmenu->field_label_tree_view));
    GtkTreeModel *child_model_field = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_comp));
    int index = 0;
    for(ifld=0;ifld<num_field;ifld++){
        kemoview_get_PSF_field_name(kemo_sgl, colorname, ifld);
        index = append_ci_item_to_tree(index, colorname->string, ifld, child_model_field);
    };
    ifld = kemoview_get_each_PSF_field_param(kemo_sgl, FIELD_SEL_FLAG);
    gtk_combo_box_set_active(GTK_COMBO_BOX(psf_gmenu->combobox_field), ifld);
    return;
}

static void init_psf_draw_field_hbox(struct kemoviewer_type *kemo_sgl,
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
    g_object_set_data(G_OBJECT(psf_gmenu->combobox_field), "kemoview", (gpointer) kemo_sgl);
	g_signal_connect(G_OBJECT(psf_gmenu->combobox_field), "changed",
                     G_CALLBACK(psf_field_select_CB), (gpointer) window);
	
    psf_gmenu->hbox_field = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(psf_gmenu->hbox_field), gtk_label_new("Field: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(psf_gmenu->hbox_field), psf_gmenu->combobox_field,
                       FALSE, FALSE, 0);
	return;
}

void update_psf_draw_component_hbox(struct kemoviewer_type *kemo_sgl, 
                                    struct psf_gtk_menu *psf_gmenu){
    char comp_name[1024];
    int icomp;

    int if_psf = kemoview_get_each_PSF_field_param(kemo_sgl, FIELD_SEL_FLAG);

    kemoview_set_each_PSF_field_param(COMPONENT_SEL_FLAG, IZERO, kemo_sgl);
    int ncomp = kemoview_get_PSF_num_component(kemo_sgl, if_psf);
    int id_coord = kemoview_get_each_PSF_field_param(kemo_sgl, COORDINATE_FLAG);
    
    clear_ci_tree_view(GTK_TREE_VIEW(psf_gmenu->comp_label_tree_view));
    GtkTreeModel *model_comp = gtk_tree_view_get_model(GTK_TREE_VIEW(psf_gmenu->comp_label_tree_view));
    GtkTreeModel *child_model_comp = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_comp));
    int index = 0;
    for(icomp=0;icomp<ncomp;icomp++){
        set_PSF_component_name(ncomp, id_coord, icomp, comp_name);
        index = append_ci_item_to_tree(index, comp_name, icomp, child_model_comp);
    };
    
    icomp = kemoview_get_each_PSF_field_param(kemo_sgl, COMPONENT_SEL_FLAG);
    gtk_combo_box_set_active(GTK_COMBO_BOX(psf_gmenu->combobox_comp), icomp);
    return;
}

static void init_psf_draw_component_hbox(struct kemoviewer_type *kemo_sgl, 
                                         struct psf_gtk_menu *psf_gmenu, GtkWidget *window){
	char comp_name[1024];
	int if_psf = kemoview_get_each_PSF_field_param(kemo_sgl, FIELD_SEL_FLAG);
	int ncomp = kemoview_get_PSF_num_component(kemo_sgl, if_psf);
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
	g_signal_connect(G_OBJECT(psf_gmenu->combobox_comp), "changed",
                     G_CALLBACK(psf_component_select_CB), (gpointer) window);
	
    psf_gmenu->hbox_comp = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(psf_gmenu->hbox_comp),
                       gtk_label_new("Component: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(psf_gmenu->hbox_comp),
                       psf_gmenu->combobox_comp, FALSE, FALSE, 0);
	return;
}


void pack_psf_menu_frame(struct psf_gtk_menu *psf_gmenu){
    GtkWidget *psf_vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->closeButton, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->hbox_psfs, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->hbox_field, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->hbox_comp, TRUE, TRUE, 0);

    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->expander_iso, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->expander_surf, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->expander_color, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->expander_vect, FALSE, TRUE, 0);

    psf_gmenu->psf_frame = wrap_into_frame_gtk("Surfaces", psf_vbox);
    return;
}

void set_psf_menu_box(struct kemoviewer_type *kemo_sgl,
                      struct kemoviewer_gl_type *kemo_gl,
                      struct updatable_widgets *updatable,
                      GtkWidget *window){
    updatable->psf_gmenu->closeButton = gtk_button_new_with_label("Close Current PSF");

    g_object_set_data(G_OBJECT(updatable->expander_evo), "parent", (gpointer) window);
    g_object_set_data(G_OBJECT(updatable->expander_evo), "psfmenu", (gpointer) updatable->psf_gmenu);
    g_object_set_data(G_OBJECT(updatable->expander_evo), "psfbox", (gpointer) updatable->psfBox);
    g_object_set_data(G_OBJECT(updatable->expander_evo), "kemoview", (gpointer) kemo_sgl);
    g_signal_connect(G_OBJECT(updatable->psf_gmenu->closeButton), "clicked",
                     G_CALLBACK(close_psf_CB), updatable->expander_evo);
	
    init_current_psf_set_hbox(kemo_sgl, updatable->psf_gmenu, window);
    init_psf_draw_field_hbox(kemo_sgl, updatable->psf_gmenu, window);
    init_psf_draw_component_hbox(kemo_sgl, updatable->psf_gmenu, window);
    init_colormap_params_4_viewer(kemo_sgl, updatable->psf_gmenu->color_vws);
	
    init_psf_menu_hbox(kemo_sgl, kemo_gl, updatable->psf_gmenu, window);
	return;
}

void set_fieldline_menu_box(struct kemoviewer_type *kemo_sgl,
                            struct fieldline_gtk_menu *fline_menu,
                            GtkWidget *window){
	GtkWidget *closeButton;

    fline_menu->closeButton = gtk_button_new_with_label("Close Current PSF");

    g_object_set_data(G_OBJECT(window), "flinemenu", (gpointer) fline_menu);
    g_object_set_data(G_OBJECT(window), "kemoview", (gpointer) kemo_sgl);
    g_signal_connect(G_OBJECT(fline_menu->closeButton), "clicked",
                     G_CALLBACK(close_fline_CB), (gpointer) window);
	
	fline_menu = (struct fieldline_gtk_menu *) malloc(sizeof(struct fieldline_gtk_menu));
    
	init_fieldline_menu_hbox(kemo_sgl, fline_menu);
    set_gtk_fieldline_menu(kemo_sgl, fline_menu);
	return;
}

void set_mesh_menu_box(struct kemoviewer_type *kemo_sgl,
                       struct updatable_widgets *updatable, 
                       GtkWidget *window){
    init_mesh_views_4_viewer(kemo_sgl, updatable->mesh_vws);
	
	/*  Set buttons */
    g_object_set_data(G_OBJECT(window), "updates", (gpointer) updatable);
    g_object_set_data(G_OBJECT(window), "kemoview", (gpointer) kemo_sgl);

    updatable->mesh_vws->closeMeshButton = gtk_button_new_with_label("Close mesh");
	g_signal_connect(G_OBJECT(updatable->mesh_vws->closeMeshButton), "clicked",
                     G_CALLBACK(close_mesh_CB), (gpointer) window);
	
    init_gtk_mesh_menu(kemo_sgl, updatable->mesh_vws, window);
	return;
}

void make_gtk_main_menu_box(struct main_buttons *mbot,
                            GtkWidget *takobox, GtkWidget *window_main,
                            struct kemoviewer_type *kemo_sgl,
                            struct kemoviewer_gl_type *kemo_gl){
	GtkWidget *entry_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_file), "parent", (gpointer)   window_main);
	g_object_set_data(G_OBJECT(entry_file), "buttons", (gpointer)  mbot);
    g_object_set_data(G_OBJECT(entry_file), "kemoview", (gpointer) kemo_sgl);
    g_object_set_data(G_OBJECT(entry_file), "kemoview_gl", (gpointer) kemo_gl);

	GtkWidget *open_Button = gtk_button_new_with_label("Open...");
	g_signal_connect(G_OBJECT(open_Button), "clicked", 
					 G_CALLBACK(open_file_CB), (gpointer)entry_file);
	
	GtkWidget *entry_image_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_image_file), "parent", (gpointer) window_main);
	GtkWidget *imageSave_Button = gtk_button_new_with_label("Save Image...");
	g_signal_connect(G_OBJECT(imageSave_Button), "clicked", 
					 G_CALLBACK(image_save_CB), (gpointer)entry_file);
	
	
	GtkWidget *label_tree_viewtype = create_fixed_label_w_index_tree();
	GtkTreeModel *model_viewtype = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_viewtype));  
	GtkTreeModel *child_model_viewtype = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_viewtype));
	int index = 0;
	index = append_ci_item_to_tree(index, "3D-View", VIEW_3D, child_model_viewtype);
	index = append_ci_item_to_tree(index, "Stereo-View", VIEW_STEREO, child_model_viewtype);
	index = append_ci_item_to_tree(index, "Map-View", VIEW_MAP, child_model_viewtype);
	index = append_ci_item_to_tree(index, "XY-View", VIEW_XY, child_model_viewtype);
	index = append_ci_item_to_tree(index, "XZ-View", VIEW_XZ, child_model_viewtype);
	index = append_ci_item_to_tree(index, "YZ-View", VIEW_YZ, child_model_viewtype);
	
	GtkWidget *combobox_viewtype = gtk_combo_box_new_with_model(child_model_viewtype);
	GtkCellRenderer *renderer_viewtype = gtk_cell_renderer_text_new();
	int iflag_mode = kemoview_get_view_type_flag(kemo_sgl);
	if(iflag_mode == VIEW_YZ){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_viewtype), 5);
	}else if(iflag_mode == VIEW_XZ){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_viewtype), 4);
	}else if(iflag_mode == VIEW_XY){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_viewtype), 3);
	}else if(iflag_mode == VIEW_MAP){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_viewtype), 2);
	}else if(iflag_mode == VIEW_STEREO){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_viewtype), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_viewtype), 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_viewtype), renderer_viewtype, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_viewtype), renderer_viewtype,
				"text", COLUMN_FIELD_NAME, NULL);
    g_object_set_data(G_OBJECT(combobox_viewtype), "kemoview",  (gpointer) kemo_sgl);
	g_signal_connect(G_OBJECT(combobox_viewtype), "changed",
                     G_CALLBACK(set_viewtype_CB), (gpointer) mbot->view_menu);
	
	
	GtkWidget *label_tree_image_fileformat = create_fixed_label_w_index_tree();
	GtkTreeModel *model_image_fileformat = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_image_fileformat));  
	GtkTreeModel *child_model_image_fileformat = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_image_fileformat));
	index = 0;
	index = append_ci_item_to_tree(index, "No Image", NO_SAVE_FILE, child_model_image_fileformat);
	index = append_ci_item_to_tree(index, "PNG", SAVE_PNG, child_model_image_fileformat);
	index = append_ci_item_to_tree(index, "BMP", SAVE_BMP, child_model_image_fileformat);
	
	mbot->ComboboxImageFormat = gtk_combo_box_new_with_model(child_model_image_fileformat);
	GtkCellRenderer *renderer_image_fileformat = gtk_cell_renderer_text_new();
	mbot->id_iamge_format = NO_SAVE_FILE;
	if(mbot->id_iamge_format == SAVE_BMP){
		gtk_combo_box_set_active(GTK_COMBO_BOX(mbot->ComboboxImageFormat), SAVE_BMP);
	} else if(mbot->id_iamge_format == SAVE_PNG){
		gtk_combo_box_set_active(GTK_COMBO_BOX(mbot->ComboboxImageFormat), SAVE_PNG);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(mbot->ComboboxImageFormat), NO_SAVE_FILE);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(mbot->ComboboxImageFormat), renderer_image_fileformat, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(mbot->ComboboxImageFormat), renderer_image_fileformat,
				"text", COLUMN_FIELD_NAME, NULL);
    g_object_set_data(G_OBJECT(mbot->ComboboxImageFormat), "kemoview",  (gpointer) kemo_sgl);
	g_signal_connect(G_OBJECT(mbot->ComboboxImageFormat), "changed",
				G_CALLBACK(set_image_fileformat_CB), entry_file);
	
	
	GtkWidget *hbox_open = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_open), gtk_label_new("File: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_open), entry_file, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_open), open_Button, FALSE, FALSE, 0);
	
	GtkWidget *hbox_image_save = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_image_save), gtk_label_new("Image file: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_image_save), mbot->ComboboxImageFormat, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_image_save), imageSave_Button, FALSE, FALSE, 0);
	
	GtkWidget *hbox_viewtype = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_viewtype), gtk_label_new("View type: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_viewtype), combobox_viewtype, FALSE, FALSE, 0);
	
    GtkWidget *hbox_axis = make_axis_menu_box(kemo_sgl, window_main);
    GtkWidget *expander_rot = init_rotation_menu_expander(kemo_sgl,
                                                          mbot->rot_gmenu,
                                                          window_main);
    
    mbot->expander_view = init_viewmatrix_menu_expander(kemo_sgl,
                                                        mbot->view_menu,
                                                        window_main);
    mbot->expander_quilt = init_quilt_menu_expander(kemo_sgl,
                                                    mbot->quilt_gmenu,
                                                    mbot->view_menu, window_main);
    mbot->expander_pref = init_preference_expander(kemo_sgl, mbot->pref_gmenu,
                                                   window_main, kemo_sgl);
    mbot->updatable->expander_evo = init_evoluaiton_menu_expander(kemo_sgl,
                                                                  mbot->updatable->evo_gmenu,
                                                                  window_main);
    activate_evolution_menu(kemo_sgl, mbot->updatable->expander_evo);

	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), hbox_open, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), hbox_image_save, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), hbox_viewtype, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), hbox_axis, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), expander_rot, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), mbot->updatable->expander_evo, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), mbot->expander_quilt, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), mbot->expander_view, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), mbot->expander_pref, FALSE, FALSE, 0);
    return;
}

