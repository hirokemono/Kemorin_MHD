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


static void init_fline_menu(struct kemoviewer_type *kemo_sgl,
                            struct fieldline_gtk_menu *fline_menu,
                            GtkWidget *window){
    if(fline_menu->iflag_flineBox > 0){gtk_widget_destroy(fline_menu->flineWin);};
    fline_menu->iflag_flineBox = kemoview_get_fline_parameters(kemo_sgl, DRAW_SWITCH);
    if(fline_menu->iflag_flineBox == 0){return;};

    fline_menu->flineWin = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(fline_menu->flineWin), "Fieldline");
    gtk_widget_set_size_request(fline_menu->flineWin, 150, -1);
    gtk_container_set_border_width(GTK_CONTAINER(fline_menu->flineWin), 5);

    set_fieldline_menu_box(kemo_sgl, fline_menu, window);
    GtkWidget *flineFrame = pack_fieldline_menu_frame(fline_menu);
    
    gtk_container_add(GTK_CONTAINER(fline_menu->flineWin), flineFrame);
    gtk_widget_show_all(fline_menu->flineWin);
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
	
    init_psf_window(kemo_sgl, kemo_gl, mbot->psf_gmenu, window_main);
    init_fline_menu(kemo_sgl, mbot->fline_menu, window_main);
    init_mesh_window(kemo_sgl, mbot->mesh_vws, mbot->meshWin);

    activate_evolution_menu(kemo_sgl, mbot->itemTEvo);
    gtk_widget_show_all(mbot->menuHbox);
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

static void close_fline_CB(GtkButton *button, gpointer user_data){
    GtkWidget *window_main = GTK_WIDGET(user_data);
    struct fieldline_gtk_menu *fline_menu = (struct fieldline_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "flinemenu");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");

	kemoview_close_fieldline_view(kemo_sgl);
	
    init_fline_menu(kemo_sgl, fline_menu, window_main);

	gtk_widget_queue_draw(window_main);
    draw_full(kemo_sgl);
};




void set_fieldline_menu_box(struct kemoviewer_type *kemo_sgl,
                            struct fieldline_gtk_menu *fline_menu,
                            GtkWidget *window){
    fline_menu->closeButton = gtk_button_new_with_label("Close Fieldline");

    g_object_set_data(G_OBJECT(window), "flinemenu", (gpointer) fline_menu);
    g_object_set_data(G_OBJECT(window), "kemoview", (gpointer) kemo_sgl);
    g_signal_connect(G_OBJECT(fline_menu->closeButton), "clicked",
                     G_CALLBACK(close_fline_CB), (gpointer) window);
	    
	init_fieldline_menu_hbox(kemo_sgl, fline_menu);
    set_gtk_fieldline_menu(kemo_sgl, fline_menu);
	return;
}

void make_gtk_main_menu_box(struct main_buttons *mbot,
                            GtkWidget *takobox, GtkWidget *window_main,
                            struct kemoviewer_type *kemo_sgl,
                            struct kemoviewer_gl_type *kemo_gl){

	
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
	
	
	GtkWidget *hbox_viewtype = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_viewtype), gtk_label_new("View type: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_viewtype), combobox_viewtype, FALSE, FALSE, 0);
	
    GtkWidget *hbox_axis = make_axis_menu_box(kemo_sgl, window_main);
    GtkWidget *expander_rot = init_rotation_menu_expander(kemo_sgl,
                                                          mbot->rot_gmenu,
                                                          window_main);
//    GtkWidget *expander_evo = init_evoluaiton_menu_expander(kemo_sgl, mbot->evo_gmenu, window_main);

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

