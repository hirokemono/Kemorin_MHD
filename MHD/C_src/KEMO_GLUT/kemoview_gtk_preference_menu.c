/*
 *  kemoview_gtk_preference_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_preference_menu.h"

static void set_image_fileformat_CB(GtkComboBox *combobox_filefmt, gpointer user_data)
{
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(combobox_filefmt), "kemoview");

    int id_img_format = gtk_selected_combobox_index(combobox_filefmt);
    kemoview_set_view_integer(IMAGE_FORMAT_FLAG, id_img_format, kemo_sgl);
	draw_full(kemo_sgl);
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

GtkWidget * init_default_image_format_menu(struct kemoviewer_type *kemo_sgl){
	
	GtkWidget *label_tree_image_fileformat = create_fixed_label_w_index_tree();
	GtkTreeModel *model_image_fileformat = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_image_fileformat));  
	GtkTreeModel *child_model_image_fileformat = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_image_fileformat));
	int index = 0;
	index = append_ci_item_to_tree(index, "No Image", NO_SAVE_FILE, child_model_image_fileformat);
	index = append_ci_item_to_tree(index, "PNG", SAVE_PNG, child_model_image_fileformat);
	index = append_ci_item_to_tree(index, "BMP", SAVE_BMP, child_model_image_fileformat);
	
	GtkWidget *ComboboxImageFormat = gtk_combo_box_new_with_model(child_model_image_fileformat);
	GtkCellRenderer *renderer_image_fileformat = gtk_cell_renderer_text_new();
	int id_img_format = kemoview_get_view_integer(kemo_sgl, IMAGE_FORMAT_FLAG);
	if(id_img_format == SAVE_BMP){
		gtk_combo_box_set_active(GTK_COMBO_BOX(ComboboxImageFormat), SAVE_BMP);
	} else if(id_img_format == SAVE_PNG){
		gtk_combo_box_set_active(GTK_COMBO_BOX(ComboboxImageFormat), SAVE_PNG);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(ComboboxImageFormat), NO_SAVE_FILE);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(ComboboxImageFormat), renderer_image_fileformat, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(ComboboxImageFormat), renderer_image_fileformat,
				"text", COLUMN_FIELD_NAME, NULL);
    g_object_set_data(G_OBJECT(ComboboxImageFormat), "kemoview",  (gpointer) kemo_sgl);
	g_signal_connect(G_OBJECT(ComboboxImageFormat), "changed",
				G_CALLBACK(set_image_fileformat_CB), NULL);
	
	
	GtkWidget *hbox_image_save = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_image_save), gtk_label_new("Image file: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_image_save), ComboboxImageFormat, FALSE, FALSE, 0);
    return wrap_into_frame_gtk("Default image format", hbox_image_save);
}


GtkWidget * init_preference_vbox(struct kemoviewer_type *kemoviewer_data,
                                 struct lightparams_view *lightparams_vws,
                                 GtkWidget *window){
    GtkWidget *pref_vbox;
    
    float color[4];
	kemoview_get_background_color(kemoviewer_data, color);
	
	/* Set buttons   */
    GtkWidget *BGselButton = gtk_button_new_with_label("Set Background");
    g_object_set_data(G_OBJECT(window), "kemoview", (gpointer) kemoviewer_data);
	g_signal_connect(G_OBJECT(BGselButton), "clicked",
                     G_CALLBACK(kemoview_gtk_BGcolorsel), (gpointer)window);
    
    GtkWidget *lighting_frame =  init_lighting_frame(kemoviewer_data, 
                                                     lightparams_vws);
    GtkWidget *Shading_frame =   shading_mode_menu_frame(kemoviewer_data);
    GtkWidget *Tube_frame =      init_tube_pref_frame(kemoviewer_data);
    GtkWidget *coastline_frame = init_coastline_pref_menu(kemoviewer_data);
    GtkWidget *Axis_frame =      init_axis_position_menu(kemoviewer_data);
    GtkWidget *FPS_frame =       init_FPS_test_menu_frame(kemoviewer_data, window);
    GtkWidget *NumThread_frame = init_num_threads_menu_frame(kemoviewer_data);
    GtkWidget *ImgFormat_frame = init_default_image_format_menu(kemoviewer_data);

    
    pref_vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_pack_start(GTK_BOX(pref_vbox), BGselButton, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_vbox), NumThread_frame, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_vbox), ImgFormat_frame, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_vbox), lighting_frame, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(pref_vbox), Tube_frame, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_vbox), Shading_frame, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_vbox), coastline_frame, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_vbox), Axis_frame, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(pref_vbox), FPS_frame, FALSE, FALSE, 0);
    return pref_vbox;
}

GtkWidget * init_preference_frame(struct kemoviewer_type *kemoviewer_data,
                                  struct lightparams_view *lightparams_vws,
                                  GtkWidget *window){
    GtkWidget *frame_pref  = gtk_frame_new("Preferences");
    gtk_frame_set_shadow_type(GTK_FRAME(frame_pref), GTK_SHADOW_IN);
    GtkWidget *pref_vbox = init_preference_vbox(kemoviewer_data, 
                                                lightparams_vws,  window);
    gtk_container_add(GTK_CONTAINER(frame_pref), pref_vbox);
    return frame_pref;
}

GtkWidget * init_preference_expander(struct kemoviewer_type *kemoviewer_data,
                                     struct lightparams_view *lightparams_vws,
                                     GtkWidget *window){
    GtkWidget *expander_pref;
    GtkWidget *pref_vbox = init_preference_vbox(kemoviewer_data,
                                                lightparams_vws, window);
    expander_pref = wrap_into_scroll_expansion_gtk("Preferences", 160, 400,
                                                   window, pref_vbox);
    return expander_pref;
}
