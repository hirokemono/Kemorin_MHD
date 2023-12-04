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
    updatable->evo_gmenu = init_evoluaiton_menu_box();
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


static void delete_kemoview_menu(struct updatable_widgets *updatable){
	gtk_widget_destroy(updatable->meshBox);
	gtk_widget_destroy(updatable->evolutionBox);
	gtk_widget_destroy(updatable->flineBox);
	gtk_widget_destroy(updatable->psfBox);
	return;
};

static void set_kemoview_menu(int id_menu[1], struct updatable_widgets *updatable,
                              GtkWidget *menuHbox, GtkWidget *window){
    int iflag;
    int iflag_draw_m = kemoview_get_draw_mesh_flag();
    int iflag_draw_f = kemoview_get_fline_parameters(DRAW_SWITCH);
    int nload_psf = kemoview_get_PSF_loaded_params(NUM_LOADED);
    
    if(nload_psf > 0){gtk_psf_menu_box(id_menu, updatable, menuHbox, window);};
    if(iflag_draw_f > 0){gtk_fieldline_menu_box(id_menu, updatable, menuHbox, window);};
    
    if(nload_psf > 0 || iflag_draw_f > 0){
        struct kv_string *file_prefix = kemoview_alloc_kvstring();
        if(nload_psf > 0){
            updatable->istep_current = kemoview_get_PSF_full_path_file_prefix(file_prefix, &iflag);
        } else {
            updatable->istep_current = kemoview_get_fline_file_step_prefix(file_prefix);
        };
        init_evoluaiton_menu_expander(updatable->istep_current, window, updatable->evo_gmenu);
        kemoview_free_kvstring(file_prefix);
    };
    
    if(iflag_draw_m > 0){gtk_mesh_menu_box(id_menu, updatable, menuHbox, window);};
    return;
};

void pack_kemoview_menu(struct updatable_widgets *updatable,
                        GtkWidget *menuHbox, GtkWidget *window){
    int iflag_draw_m = kemoview_get_draw_mesh_flag();
    int iflag_draw_f = kemoview_get_fline_parameters(DRAW_SWITCH);
    int nload_psf = kemoview_get_PSF_loaded_params(NUM_LOADED);

    updatable->psfBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	if(nload_psf > 0){
        GtkWidget *frame = pack_psf_menu_frame(updatable->psf_gmenu);
        gtk_box_pack_start(GTK_BOX(updatable->psfBox), frame, FALSE, FALSE, 0);
        gtk_widget_show_all(updatable->psfBox);
    };
	
    updatable->flineBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    if(iflag_draw_f > 0){
        GtkWidget *frame = pack_fieldline_menu_frame(updatable->fline_menu);
        gtk_box_pack_start(GTK_BOX(updatable->flineBox), frame, FALSE, FALSE, 0);
        gtk_widget_show_all(updatable->flineBox);
    };
	
    updatable->evolutionBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	if(nload_psf > 0 || iflag_draw_f > 0){
		if(nload_psf > 0){
            GtkWidget *expand_psf_evo = pack_evoluaiton_menu_expander(updatable->istep_current,
                                                                      window, updatable->evo_gmenu);
            gtk_box_pack_start(GTK_BOX(updatable->evolutionBox), expand_psf_evo, FALSE, FALSE, 0);
			gtk_box_pack_start(GTK_BOX(updatable->psfBox), updatable->evolutionBox, FALSE, FALSE, 0);
		} else {
            GtkWidget *expand_fline_evo = pack_evoluaiton_menu_expander(updatable->istep_current,
                                                                        window, updatable->evo_gmenu);
            gtk_box_pack_start(GTK_BOX(updatable->evolutionBox), expand_fline_evo, FALSE, FALSE, 0);
			gtk_box_pack_start(GTK_BOX(updatable->flineBox), updatable->evolutionBox, FALSE, FALSE, 0);
		};
	};
	
    updatable->meshBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    if(iflag_draw_m > 0){
        GtkWidget *frame_mesh = pack_gtk_mesh_menu(updatable->mesh_vws, window);
        gtk_box_pack_start(GTK_BOX(updatable->meshBox), frame_mesh, FALSE, FALSE, 0);
	};
	
	gtk_box_pack_start(GTK_BOX(menuHbox), updatable->psfBox, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(menuHbox), updatable->flineBox, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(menuHbox), updatable->meshBox, FALSE, FALSE, 0);
	
	gtk_window_resize(GTK_WINDOW(window), 240, 200);
	
	
	gtk_widget_show_all(menuHbox);
	if(nload_psf == 0) gtk_widget_hide(updatable->psfBox);
	if(iflag_draw_f == 0) gtk_widget_hide(updatable->flineBox);
	if(iflag_draw_m == 0) gtk_widget_hide(updatable->meshBox);
	return;
};


void update_kemoview_menu(int id_menu[1], struct updatable_widgets *updatable,
                          GtkWidget *menuHbox, GtkWidget *window){
    int id = 1 - id_menu[0];
    delete_kemoview_menu(updatable);
    set_kemoview_menu(id_menu, updatable, menuHbox, window);
    pack_kemoview_menu(updatable, menuHbox, window);
    id_menu[0] = id;
    printf("menu id: %d\n", id_menu[0]);
    return;
};


void open_kemoviewer_file_glfw(struct kv_string *filename, struct main_buttons *mbot,
			GtkWidget *window_main){
    struct kv_string *file_prefix = kemoview_alloc_kvstring();
    struct kv_string *stripped_ext = kemoview_alloc_kvstring();
	int iflag_datatype = kemoview_set_data_format_flag(filename, file_prefix, stripped_ext);
	
	printf("file name: %s\n", filename->string);
	printf("file_prefix %s\n", file_prefix->string);
	printf("stripped_ext %s\n", stripped_ext->string);
    kemoview_free_kvstring(stripped_ext);
    kemoview_free_kvstring(file_prefix);
	
	iflag_datatype = kemoview_open_data(filename);
    kemoview_free_kvstring(filename);
	
    update_kemoview_menu(mbot->id_current, mbot->updatable,
                         mbot->menuHbox, window_main);
	gtk_widget_queue_draw(window_main);
	draw_full();
	return;
};

static void set_viewtype_CB(GtkComboBox *combobox_viewtype, gpointer user_data)
{
    struct view_widgets *view_menu = (struct view_widgets *) user_data;

	int index_mode = gtk_selected_combobox_index(combobox_viewtype);
	
    set_GLFW_viewtype_mode(index_mode);
    kemoview_set_viewtype(index_mode);
    draw_full();
    
    if(kemoview_get_view_type_flag() == VIEW_STEREO){
        gtk_widget_set_sensitive(view_menu->Frame_stereo, TRUE);
    }else{
        gtk_widget_set_sensitive(view_menu->Frame_stereo, FALSE);
    };
	return;
};

static void set_image_fileformat_CB(GtkComboBox *combobox_filefmt, gpointer user_data)
{
	struct main_buttons *mbot = (struct main_buttons *) g_object_get_data(G_OBJECT(user_data), "buttons");

	mbot->id_iamge_format = gtk_selected_combobox_index(combobox_filefmt);
	draw_full();
	return;
};

static void image_save_CB(GtkButton *button, gpointer user_data){
	struct main_buttons *mbot = (struct main_buttons *) g_object_get_data(G_OBJECT(user_data), "buttons");
	int iflag_set = kemoview_gtk_save_file_select(button, user_data);
	int id_imagefmt_by_input;
    int i_quilt;
    
	if(iflag_set == IZERO) return;
	
    int iflag_quilt = kemoview_get_quilt_nums(ISET_QUILT_MODE);
    int npix_x = kemoview_get_view_integer(ISET_PIXEL_X);
    int npix_y = kemoview_get_view_integer(ISET_PIXEL_Y);
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
        int nimg_column = kemoview_get_quilt_nums(ISET_QUILT_COLUMN);
        int nimg_raw = kemoview_get_quilt_nums(ISET_QUILT_RAW);
        unsigned char *quilt_image = kemoview_alloc_RGB_buffer_to_bmp((nimg_column * npix_x),
                                                                      (nimg_raw * npix_y));
        for(i_quilt=0;i_quilt<(nimg_column*nimg_raw);i_quilt++){
            kemoview_set_quilt_nums(ISET_QUILT_COUNT, i_quilt);
            draw_quilt();
            kemoview_get_gl_buffer_to_bmp(npix_x, npix_y, image);
            kemoview_add_quilt_img(image, quilt_image);
        };
        kemoview_write_window_to_file(id_imagefmt_by_input, file_prefix,
                                      (nimg_column * npix_x),
                                      (nimg_raw * npix_y), quilt_image);
        free(quilt_image);
        printf("quilt! %d x %d\n", nimg_column, nimg_raw);
        draw_full();
    }
    free(image);
    kemoview_free_kvstring(file_prefix);
	
	return;
};

static void current_psf_select_CB(GtkComboBox *combobox_sfcolor, gpointer user_data)
{
    GtkWidget *menuHbox = GTK_WIDGET(user_data);
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct updatable_widgets *updatable = (struct updatable_widgets *) g_object_get_data(G_OBJECT(user_data), "updates");
    int *id_menu = (int *) g_object_get_data(G_OBJECT(user_data), "current");

    int index_mode = gtk_selected_combobox_index(combobox_sfcolor);
	
	kemoview_set_PSF_loaded_params(SET_CURRENT, index_mode);
	
	update_kemoview_menu(id_menu, updatable, menuHbox, window_main);

	gtk_widget_queue_draw(window_main);
	draw_full();
	return;
};

static void close_psf_CB(GtkButton *button, gpointer user_data){
    GtkWidget *menuHbox = GTK_WIDGET(user_data);
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct updatable_widgets *updatable = (struct updatable_widgets *) g_object_get_data(G_OBJECT(user_data), "updates");
    int *id_menu = (int *) g_object_get_data(G_OBJECT(user_data), "current");

    int num_loaded = kemoview_close_PSF_view();

    set_GLFW_viewtype_mode(VIEW_3D);
    kemoview_set_viewtype(VIEW_3D);
	
	update_kemoview_menu(id_menu, updatable, menuHbox, window_main);

	gtk_widget_queue_draw(window_main);
	draw_full();
};

static void close_fline_CB(GtkButton *button, gpointer user_data){
    GtkWidget *menuHbox = GTK_WIDGET(user_data);
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
    struct updatable_widgets *updatable = (struct updatable_widgets *) g_object_get_data(G_OBJECT(user_data), "updates");
    int *id_menu = (int *) g_object_get_data(G_OBJECT(user_data), "current");

	kemoview_close_fieldline_view();
	
	update_kemoview_menu(id_menu, updatable, menuHbox, window_main);

	gtk_widget_queue_draw(window_main);
	draw_full();
};

static void close_mesh_CB(GtkButton *button, gpointer user_data){
    GtkWidget *menuHbox = GTK_WIDGET(user_data);
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
    struct updatable_widgets *updatable = (struct updatable_widgets *) g_object_get_data(G_OBJECT(user_data), "updates");
    int *id_menu = (int *) g_object_get_data(G_OBJECT(user_data), "current");

	kemoview_close_mesh_view();
	dealloc_mesh_views_4_viewer(updatable->mesh_vws);
	
    update_kemoview_menu(id_menu, updatable, menuHbox, window_main);

	gtk_widget_queue_draw(window_main);
	draw_full();
	return;
};

static void psf_field_select_CB(GtkComboBox *combobox_field, gpointer user_data)
{
    GtkWidget *menuHbox = GTK_WIDGET(user_data);
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
    struct updatable_widgets *updatable = (struct updatable_widgets *) g_object_get_data(G_OBJECT(user_data), "updates");
    int *id_menu = (int *) g_object_get_data(G_OBJECT(user_data), "current");

    int index_mode = gtk_selected_combobox_index(combobox_field);
    
	kemoview_set_each_PSF_field_param(FIELD_SEL_FLAG, index_mode);
		
	update_kemoview_menu(id_menu, updatable, menuHbox, window_main);
    set_vector_plot_availablity(updatable->psf_gmenu);

    gtk_widget_queue_draw(window_main);
	draw_full();
	return;
};

static void psf_component_select_CB(GtkComboBox *combobox_comp, gpointer user_data)
{
    GtkWidget *menuHbox = GTK_WIDGET(user_data);
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
    struct updatable_widgets *updatable = (struct updatable_widgets *) g_object_get_data(G_OBJECT(user_data), "updates");
    int *id_menu = (int *) g_object_get_data(G_OBJECT(user_data), "current");

    int index_mode = gtk_selected_combobox_index(combobox_comp);
	
	kemoview_set_each_PSF_field_param(COMPONENT_SEL_FLAG, index_mode);
		
    update_kemoview_menu(id_menu, updatable, menuHbox, window_main);

	gtk_widget_queue_draw(window_main);
	draw_full();
	return;
};


static void open_file_CB(GtkButton *button, gpointer user_data){
    struct kv_string *filename;
	
	int iflag_set = kemoview_gtk_read_file_select(button, user_data);
	if(iflag_set == IZERO) return;
	GtkEntry *entry = GTK_ENTRY(user_data);
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct main_buttons *mbot = (struct main_buttons *) g_object_get_data(G_OBJECT(user_data), "buttons");
	filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
	
	open_kemoviewer_file_glfw(filename, mbot, window_main);
	return;
};


static GtkWidget * init_current_psf_set_hbox(int id_menu[1], struct updatable_widgets *updatable,
                                             GtkWidget *menuHbox, GtkWidget *window){
	GtkWidget *hbox_psfs;
	
	int index = 0;
	int ipsf;
	
	struct kv_string *stripped_filehead;
	char label_tmp[512];
	int id_current = kemoview_get_PSF_loaded_params(SET_CURRENT);
	int index_current = 0;

    updatable->psf_gmenu->num_psfs = 0;
	for (ipsf=0; ipsf< kemoview_get_PSF_loaded_params(MAX_LOADED); ipsf++){
		if(kemoview_get_PSF_loaded_flag(ipsf) > 0) {
            updatable->psf_gmenu->num_psfs = updatable->psf_gmenu->num_psfs + 1;
        };
	};
	
    hbox_psfs = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    if(updatable->psf_gmenu->num_psfs > 1){
        GtkWidget *label_tree_psfs = create_fixed_label_w_index_tree();
        GtkTreeModel *model_psfs = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_psfs));
        GtkTreeModel *child_model_psfs = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_psfs));
        index = 0;
        for (ipsf=0; ipsf< kemoview_get_PSF_loaded_params(MAX_LOADED); ipsf++){
            if(ipsf == id_current) {index_current = index;};
            if(kemoview_get_PSF_loaded_flag(ipsf) > 0) {
                kemoview_set_PSF_loaded_params(SET_CURRENT, ipsf);
                stripped_filehead = kemoview_alloc_kvstring();
                kemoview_get_PSF_file_prefix(stripped_filehead);
                sprintf(label_tmp, "%d: %s", ipsf, stripped_filehead->string);
                index = append_ci_item_to_tree(index, label_tmp,
                                               ipsf, child_model_psfs);
                kemoview_free_kvstring(stripped_filehead);
            };
            kemoview_set_PSF_loaded_params(SET_CURRENT, id_current);
        };

        updatable->psf_gmenu->combobox_psfs = gtk_combo_box_new_with_model(child_model_psfs);
        GtkCellRenderer *renderer_psfs = gtk_cell_renderer_text_new();
        gtk_combo_box_set_active(GTK_COMBO_BOX(updatable->psf_gmenu->combobox_psfs), index_current);
        gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(updatable->psf_gmenu->combobox_psfs), renderer_psfs, TRUE);
        gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(updatable->psf_gmenu->combobox_psfs), renderer_psfs,
                                       "text", COLUMN_FIELD_NAME, NULL);
        
        g_object_set_data(G_OBJECT(menuHbox), "parent", (gpointer) window);
        g_object_set_data(G_OBJECT(menuHbox), "updates", (gpointer) updatable);
        g_object_set_data(G_OBJECT(menuHbox), "current", (gpointer) id_menu);
        g_signal_connect(G_OBJECT(updatable->psf_gmenu->combobox_psfs), "changed",
                         G_CALLBACK(current_psf_select_CB), (gpointer) menuHbox);
        
        gtk_box_pack_start(GTK_BOX(hbox_psfs), gtk_label_new("Current PSF: "),
                           FALSE, FALSE, 0);
        gtk_box_pack_start(GTK_BOX(hbox_psfs), updatable->psf_gmenu->combobox_psfs,
                           FALSE, FALSE, 0);
    }
    return hbox_psfs;
}

static GtkWidget * init_psf_draw_field_hbox(int id_menu[1], struct updatable_widgets *updatable,
                                            GtkWidget *menuHbox, GtkWidget *window){
	GtkWidget *hbox_field;
	
	struct kv_string *colorname = kemoview_alloc_kvstring();
	int num_field = kemoview_get_each_PSF_field_param(NUM_FIELD_FLAG);
	int if_psf =    kemoview_get_each_PSF_field_param(FIELD_SEL_FLAG);
	int ifld;
	
	GtkWidget *label_tree_field = create_fixed_label_w_index_tree();
	GtkTreeModel *model_field = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_field));  
	GtkTreeModel *child_model_field = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_field));
	
	int index = 0;
	for(ifld=0;ifld<num_field;ifld++){
		kemoview_get_PSF_field_name(colorname, ifld);
		index = append_ci_item_to_tree(index, colorname->string, ifld, child_model_field);
	};

	GtkCellRenderer *renderer_field = gtk_cell_renderer_text_new();
	updatable->psf_gmenu->combobox_field = gtk_combo_box_new_with_model(child_model_field);
	gtk_combo_box_set_active(GTK_COMBO_BOX(updatable->psf_gmenu->combobox_field), if_psf);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(updatable->psf_gmenu->combobox_field), renderer_field, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(updatable->psf_gmenu->combobox_field), renderer_field,
                                   "text", COLUMN_FIELD_NAME, NULL);
    
    g_object_set_data(G_OBJECT(menuHbox), "parent", (gpointer) window);
    g_object_set_data(G_OBJECT(menuHbox), "updates", (gpointer) updatable);
    g_object_set_data(G_OBJECT(menuHbox), "current", (gpointer) id_menu);
	g_signal_connect(G_OBJECT(updatable->psf_gmenu->combobox_field), "changed",
                     G_CALLBACK(psf_field_select_CB), (gpointer) menuHbox);
	
	hbox_field = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_field), gtk_label_new("Field: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_field), updatable->psf_gmenu->combobox_field, FALSE, FALSE, 0);
	return hbox_field;
}

static GtkWidget * init_psf_draw_component_hbox(int id_menu[1], struct updatable_widgets *updatable,
                                                GtkWidget *menuHbox, GtkWidget *window){
    GtkWidget *hbox_comp = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	
	char comp_name[1024];
	int if_psf = kemoview_get_each_PSF_field_param(FIELD_SEL_FLAG);
	int ncomp = kemoview_get_PSF_num_component(if_psf);
	int icomp;
	
    GtkWidget *label_tree_comp = create_fixed_label_w_index_tree();
	GtkTreeModel *model_comp = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_comp));  
	GtkTreeModel *child_model_comp = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_comp));
	
	int id_coord = kemoview_get_each_PSF_field_param(COORDINATE_FLAG);
	int index = 0;
	for(icomp=0;icomp<ncomp;icomp++){
		set_PSF_component_name(ncomp, id_coord, icomp, comp_name);
		index = append_ci_item_to_tree(index, comp_name, icomp, child_model_comp);
	};
	
	icomp = kemoview_get_each_PSF_field_param(COMPONENT_SEL_FLAG);
	GtkCellRenderer *renderer_comp = gtk_cell_renderer_text_new();
    updatable->psf_gmenu->combobox_comp = gtk_combo_box_new_with_model(child_model_comp);
	gtk_combo_box_set_active(GTK_COMBO_BOX(updatable->psf_gmenu->combobox_comp), icomp);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(updatable->psf_gmenu->combobox_comp), renderer_comp, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(updatable->psf_gmenu->combobox_comp), renderer_comp,
				"text", COLUMN_FIELD_NAME, NULL);

    g_object_set_data(G_OBJECT(menuHbox), "parent", (gpointer) window);
    g_object_set_data(G_OBJECT(menuHbox), "updates", (gpointer) updatable);
    g_object_set_data(G_OBJECT(menuHbox), "current", (gpointer) id_menu);
	g_signal_connect(G_OBJECT(updatable->psf_gmenu->combobox_comp), "changed",
                     G_CALLBACK(psf_component_select_CB), (gpointer) menuHbox);
	
	gtk_box_pack_start(GTK_BOX(hbox_comp), gtk_label_new("Component: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_comp), updatable->psf_gmenu->combobox_comp, FALSE, FALSE, 0);
	return hbox_comp;
}


GtkWidget * pack_psf_menu_frame(struct psf_gtk_menu *psf_gmenu){
    GtkWidget *psf_vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->closeButton, FALSE, FALSE, 0);
    if(psf_gmenu->num_psfs > 1){
        gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->hbox_psfs, TRUE, TRUE, 0);
    }
    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->hbox_field, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->hbox_comp, TRUE, TRUE, 0);

    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->expander_iso, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->expander_surf, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->expander_color, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(psf_vbox), psf_gmenu->expander_vect, FALSE, TRUE, 0);

    GtkWidget *frame = wrap_into_frame_gtk("Surfaces", psf_vbox);
    return frame;
}

void gtk_psf_menu_box(int id_menu[1], struct updatable_widgets  *updatable,
                      GtkWidget *menuHbox, GtkWidget *window){
    updatable->psf_gmenu->closeButton = gtk_button_new_with_label("Close Current PSF");

    g_object_set_data(G_OBJECT(menuHbox), "parent", (gpointer) window);
    g_object_set_data(G_OBJECT(menuHbox), "updates", (gpointer) updatable);
    g_object_set_data(G_OBJECT(menuHbox), "current", (gpointer) id_menu);
    g_signal_connect(G_OBJECT(updatable->psf_gmenu->closeButton), "clicked",
                     G_CALLBACK(close_psf_CB), menuHbox);
	
    updatable->psf_gmenu->hbox_psfs = init_current_psf_set_hbox(id_menu,updatable,
                                                                menuHbox, window);
    updatable->psf_gmenu->hbox_field = init_psf_draw_field_hbox(id_menu, updatable,
                                                                menuHbox, window);
    updatable->psf_gmenu->hbox_comp = init_psf_draw_component_hbox(id_menu, updatable,
                                                                   menuHbox, window);
	
	init_colormap_views_4_viewer(updatable->psf_gmenu->color_vws);
	
    init_psf_menu_hbox(updatable->psf_gmenu, window);
	return;
}

void gtk_fieldline_menu_box(int id_menu[1], struct updatable_widgets *updatable,
                            GtkWidget *menuHbox, GtkWidget *window){
	GtkWidget *closeButton;

    updatable->fline_menu->closeButton = gtk_button_new_with_label("Close Current PSF");

    g_object_set_data(G_OBJECT(menuHbox), "parent", (gpointer) window);
    g_object_set_data(G_OBJECT(menuHbox), "updates", (gpointer) updatable);
    g_object_set_data(G_OBJECT(menuHbox), "current", (gpointer) id_menu);
    g_signal_connect(G_OBJECT(updatable->fline_menu->closeButton), "clicked",
                     G_CALLBACK(close_fline_CB), menuHbox);
	
	updatable->fline_menu = (struct fieldline_gtk_menu *) malloc(sizeof(struct fieldline_gtk_menu));
    
    
	init_fieldline_menu_hbox(updatable->fline_menu);
    set_gtk_fieldline_menu(updatable->fline_menu);
	return;
}

void gtk_mesh_menu_box(int id_menu[1], struct updatable_widgets *updatable,
                       GtkWidget *menuHbox, GtkWidget *window){
    init_mesh_views_4_viewer(updatable->mesh_vws);
	
	/*  Set buttons */
	g_object_set_data(G_OBJECT(menuHbox), "parent", (gpointer) window);
    g_object_set_data(G_OBJECT(menuHbox), "updates", (gpointer) updatable);
    g_object_set_data(G_OBJECT(menuHbox), "current", (gpointer) id_menu);

    updatable->mesh_vws->closeMeshButton = gtk_button_new_with_label("Close mesh");
	g_signal_connect(G_OBJECT(updatable->mesh_vws->closeMeshButton), "clicked",
                     G_CALLBACK(close_mesh_CB), (gpointer) menuHbox);
	
    init_gtk_mesh_menu(updatable->mesh_vws, window);
	return;
}

void make_gtk_main_menu_box(struct main_buttons *mbot, GtkWidget *takobox,
                            GtkWidget *window_main,
                            struct kemoviewer_type *kemoviewer_data){
	GtkWidget *entry_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_file), "parent", (gpointer) window_main);
	g_object_set_data(G_OBJECT(entry_file), "buttons", (gpointer) mbot);
	
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
	int iflag_mode = kemoview_get_view_type_flag();
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
	
    GtkWidget *hbox_axis = make_axis_menu_box(window_main);
    GtkWidget *expander_rot = init_rotation_menu_expander(mbot->rot_gmenu, window_main);
    
    mbot->expander_view = init_viewmatrix_menu_expander(mbot->view_menu, window_main);
    mbot->expander_quilt = init_quilt_menu_expander(mbot->quilt_gmenu,
                                                    mbot->view_menu, window_main);
    mbot->expander_pref = init_preference_expander(mbot->pref_gmenu, window_main,
                                                   kemoviewer_data);

    
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), hbox_open, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), hbox_image_save, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), hbox_viewtype, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), hbox_axis, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), expander_rot, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), mbot->expander_quilt, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), mbot->expander_view, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), mbot->expander_pref, FALSE, FALSE, 0);
    return;
}

