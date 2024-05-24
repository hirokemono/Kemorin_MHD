/*
 *  kemoview_gtk_evolution_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_evolution_menu.h"

int i_fps_r = 24;
struct evolution_gtk_menu * init_evoluaiton_menu_box(struct kemoviewer_type *kemo_sgl){
	struct evolution_gtk_menu *evo_gmenu
			= (struct evolution_gtk_menu *)  malloc(sizeof(struct evolution_gtk_menu));
	evo_gmenu->id_fmt_evo = 0;
	
	evo_gmenu->istart_evo = 0;
	evo_gmenu->iend_evo =   0;
	evo_gmenu->inc_evo =    1;
	
	kemoview_set_object_property_flags(0, TIME_LABEL_SWITCH, kemo_sgl);
	kemoview_set_object_property_flags(0, FILE_STEP_LABEL_SWITCH, kemo_sgl);
	return evo_gmenu;
};

static void evolution_view_CB(GtkButton *button, gpointer user_data){
	GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "evoWindow"));
	struct evolution_gtk_menu *evo_gmenu 
			= (struct evolution_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "evolution");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");

    draw_evolution_views(kemo_sgl,
                         evo_gmenu->istart_evo,
                         evo_gmenu->iend_evo,
                         evo_gmenu->inc_evo);

    gtk_widget_destroy(window);
	return;
};

static void draw_time_switch_CB(GObject *switch_bar, GParamSpec *pspec, gpointer data){
	struct evolution_gtk_menu *evo_gmenu 
			= (struct evolution_gtk_menu *) g_object_get_data(G_OBJECT(data), "evolution");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(data), "kemoview");
    int iflag = gtk_switch_get_state(GTK_SWITCH(switch_bar));
    kemoview_set_object_property_flags(TIME_LABEL_SWITCH, iflag, kemo_sgl);
    	
    draw_full(kemo_sgl);
	return;
};
static void draw_fileindex_switch_CB(GObject *switch_bar, GParamSpec *pspec, gpointer data){
	struct evolution_gtk_menu *evo_gmenu 
			= (struct evolution_gtk_menu *) g_object_get_data(G_OBJECT(data), "evolution");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(data), "kemoview");
    int iflag = gtk_switch_get_state(GTK_SWITCH(switch_bar));
    kemoview_set_object_property_flags(FILE_STEP_LABEL_SWITCH, iflag, kemo_sgl);
	
    draw_full(kemo_sgl);
	return;
};

static void evolution_save_CB(GtkButton *button, gpointer user_data){
	int id_image;
	GtkEntry *entry = GTK_ENTRY(user_data);
	GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "evoWindow"));
	struct evolution_gtk_menu *evo_gmenu 
			= (struct evolution_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "evolution");
    struct kemoviewer_type *kemo_sgl
            = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");
                             
    kemoview_gtk_save_file_select(button, G_OBJECT(entry));
	struct kv_string *filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
	struct kv_string *stripped_ext = kemoview_alloc_kvstring();
	struct kv_string *file_prefix = kemoview_alloc_kvstring();
	
	kemoview_get_ext_from_file_name(filename, file_prefix, stripped_ext);
	id_image = kemoview_set_image_file_format_id(stripped_ext);
	if(id_image < 0) {
		id_image = evo_gmenu->id_fmt_evo;
	};
	if(id_image == 0) return;
	kemoview_free_kvstring(filename);
	kemoview_free_kvstring(stripped_ext);
	
	printf("header: %s\n", file_prefix->string);
	printf("steps: %d %d %d\n", evo_gmenu->istart_evo, evo_gmenu->iend_evo, evo_gmenu->inc_evo);
    sel_write_evolution_views(kemo_sgl, id_image, file_prefix, i_fps_r,
                              evo_gmenu->istart_evo, evo_gmenu->iend_evo,
                              evo_gmenu->inc_evo);
    kemoview_free_kvstring(file_prefix);
	
    gtk_widget_destroy(window);
	return;
};

static void evo_start_step_CB(GtkWidget *entry, gpointer user_data)
{
	struct evolution_gtk_menu *evo_gmenu 
			= (struct evolution_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "evolution");
	evo_gmenu->istart_evo = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
/*	printf("radius %d\n", radius);*/
}
static void evo_end_step_CB(GtkWidget *entry, gpointer user_data)
{
	struct evolution_gtk_menu *evo_gmenu 
			= (struct evolution_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "evolution");
	evo_gmenu->iend_evo = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
/*	printf("radius %d\n", radius);*/
}
static void evo_increment_CB(GtkWidget *entry, gpointer user_data)
{
	struct evolution_gtk_menu *evo_gmenu 
			= (struct evolution_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "evolution");
	evo_gmenu->inc_evo = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
/*	printf("radius %d\n", radius);*/
}

static void evo_FPS_CB(GtkWidget *entry, gpointer user_data)
{
    struct evolution_gtk_menu *evo_gmenu
            = (struct evolution_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "evolution");
    evo_gmenu->i_FPS = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
/*    printf("radius %d\n", radius);*/
}

static void set_evo_fileformat_CB(GtkComboBox *combobox_filefmt, gpointer user_data)
{
	struct evolution_gtk_menu *evo_gmenu 
			= (struct evolution_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "evolution");
	evo_gmenu->id_fmt_evo = gtk_selected_combobox_index(combobox_filefmt);
	return;
};



static void set_evoluaiton_menu_expander(struct kemoviewer_type *kemo_sgl,
                                         GtkWidget *window,
                                         struct evolution_gtk_menu *evo_gmenu){
    evo_gmenu->entry_evo_file = gtk_entry_new();
    g_object_set_data(G_OBJECT(evo_gmenu->entry_evo_file), "evoWindow", (gpointer) window);
    g_object_set_data(G_OBJECT(evo_gmenu->entry_evo_file), "evolution", (gpointer) evo_gmenu);
    g_object_set_data(G_OBJECT(evo_gmenu->entry_evo_file), "kemoview",  (gpointer) kemo_sgl);

    GtkWidget *label_tree_evo_fileformat = create_fixed_label_w_index_tree();
    GtkTreeModel *model_evo_fileformat = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_evo_fileformat));
    GtkTreeModel *child_model_evo_fileformat = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_evo_fileformat));
    
    int index = 0;
    index = append_ci_item_to_tree(index, "No Image", NO_SAVE_FILE,  child_model_evo_fileformat);
    index = append_ci_item_to_tree(index, "PNG",      SAVE_PNG,      child_model_evo_fileformat);
    index = append_ci_item_to_tree(index, "BMP",      SAVE_BMP,      child_model_evo_fileformat);
#ifdef FFMPEG
    index = append_ci_item_to_tree(index, "Movie",    SAVE_QT_MOVIE, child_model_evo_fileformat);
#endif

    GtkCellRenderer *renderer_evo_fileformat = gtk_cell_renderer_text_new();
    evo_gmenu->combobox_evo_fileformat = gtk_combo_box_new_with_model(child_model_evo_fileformat);
    evo_gmenu->id_fmt_evo = NO_SAVE_FILE;
    gtk_combo_box_set_active(GTK_COMBO_BOX(evo_gmenu->combobox_evo_fileformat), 0);
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(evo_gmenu->combobox_evo_fileformat),
                               renderer_evo_fileformat, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(evo_gmenu->combobox_evo_fileformat),
                                   renderer_evo_fileformat, "text", COLUMN_FIELD_NAME, NULL);
    g_signal_connect(G_OBJECT(evo_gmenu->combobox_evo_fileformat), "changed",
                     G_CALLBACK(set_evo_fileformat_CB), (gpointer) evo_gmenu->entry_evo_file);
    
    evo_gmenu->switch_timelabel = gtk_switch_new();
    if(kemoview_get_object_property_flags(kemo_sgl, TIME_LABEL_SWITCH) == 0){
        gtk_switch_set_active(GTK_SWITCH(evo_gmenu->switch_timelabel), FALSE);
    } else {
        gtk_switch_set_active(GTK_SWITCH(evo_gmenu->switch_timelabel), TRUE);
    };
    g_signal_connect(G_OBJECT(evo_gmenu->switch_timelabel), "notify::active",
                     G_CALLBACK(draw_time_switch_CB), (gpointer) evo_gmenu->entry_evo_file);
    
    evo_gmenu->switch_fileindex = gtk_switch_new();
    if(kemoview_get_object_property_flags(kemo_sgl, FILE_STEP_LABEL_SWITCH) == 0){
        gtk_switch_set_active(GTK_SWITCH(evo_gmenu->switch_fileindex), FALSE);
    } else {
        gtk_switch_set_active(GTK_SWITCH(evo_gmenu->switch_fileindex), TRUE);
    };
    g_signal_connect(G_OBJECT(evo_gmenu->switch_fileindex), "notify::active",
                     G_CALLBACK(draw_fileindex_switch_CB), (gpointer) evo_gmenu->entry_evo_file);
    
    evo_gmenu->evoView_Button = gtk_button_new_with_label("View Evolution");
    g_signal_connect(G_OBJECT(evo_gmenu->evoView_Button), "clicked",
                     G_CALLBACK(evolution_view_CB), (gpointer) evo_gmenu->entry_evo_file);
    evo_gmenu->evoSave_Button = gtk_button_new_with_label("Save Evolution");
    g_signal_connect(G_OBJECT(evo_gmenu->evoSave_Button), "clicked",
                     G_CALLBACK(evolution_save_CB), (gpointer) evo_gmenu->entry_evo_file);
    
    int file_fmt;
    struct kv_string *image_prefix = kemoview_init_kvstring_by_string("CalypsoView");
    evo_gmenu->istart_evo = kemoview_get_PSF_full_path_file_prefix(kemo_sgl, image_prefix, &file_fmt);
    evo_gmenu->iend_evo = evo_gmenu->istart_evo;
    evo_gmenu->inc_evo = 1;
    kemoview_free_kvstring(image_prefix);
    
    GtkAdjustment *adj_evo_start = gtk_adjustment_new(evo_gmenu->istart_evo, 0, evo_gmenu->istart_evo*1000, 1, 1, 0.0);
    evo_gmenu->spin_evo_start = gtk_spin_button_new(GTK_ADJUSTMENT(adj_evo_start), 1, 0);
    g_signal_connect(evo_gmenu->spin_evo_start, "value-changed",
                     G_CALLBACK(evo_start_step_CB), (gpointer) evo_gmenu->entry_evo_file);
    
    GtkAdjustment *adj_evo_end = gtk_adjustment_new(evo_gmenu->iend_evo, 0.00, evo_gmenu->iend_evo*1000, 1, 1, 0.0);
    evo_gmenu->spin_evo_end = gtk_spin_button_new(GTK_ADJUSTMENT(adj_evo_end), 1, 0);
    g_signal_connect(evo_gmenu->spin_evo_end, "value-changed",
                     G_CALLBACK(evo_end_step_CB), (gpointer) evo_gmenu->entry_evo_file);
    
    GtkAdjustment *adj_evo_increment = gtk_adjustment_new(evo_gmenu->inc_evo, 0, evo_gmenu->iend_evo*100, 1, 1, 0.0);
    evo_gmenu->spin_evo_increment = gtk_spin_button_new(GTK_ADJUSTMENT(adj_evo_increment), 1, 0);
    g_signal_connect(evo_gmenu->spin_evo_increment, "value-changed",
                     G_CALLBACK(evo_increment_CB), (gpointer) evo_gmenu->entry_evo_file);
    
    evo_gmenu->i_FPS = 30;
    GtkAdjustment *adj_evo_FPS = gtk_adjustment_new(evo_gmenu->i_FPS, 1, 180, 1, 1, 0.0);
    evo_gmenu->spin_evo_FPS = gtk_spin_button_new(GTK_ADJUSTMENT(adj_evo_FPS), 0, 1);
    gtk_spin_button_set_digits(GTK_SPIN_BUTTON(evo_gmenu->spin_evo_FPS), 0);
    gtk_entry_set_width_chars(GTK_ENTRY(evo_gmenu->spin_evo_FPS), 6);
    g_signal_connect(evo_gmenu->spin_evo_FPS, "value-changed",
                     G_CALLBACK(evo_FPS_CB), (gpointer) evo_gmenu->entry_evo_file);
    return;
}

static GtkWidget * pack_evoluaiton_menu_box(struct evolution_gtk_menu *evo_gmenu){
    GtkWidget *evo_box;
    
	GtkWidget *hbox_time = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_time), gtk_label_new("Draw time: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_time), evo_gmenu->switch_timelabel, FALSE, FALSE, 0);
	
	GtkWidget *hbox_fileindex = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_fileindex), gtk_label_new("Draw file step: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_fileindex), evo_gmenu->switch_fileindex, FALSE, FALSE, 0);
	
	GtkWidget *hbox_evo_start = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_evo_start), gtk_label_new("Start step: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_evo_start), evo_gmenu->spin_evo_start, TRUE, TRUE, 0);
	GtkWidget *hbox_evo_end = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_evo_end), gtk_label_new("End step: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_evo_end), evo_gmenu->spin_evo_end, TRUE, TRUE, 0);
	GtkWidget *hbox_evo_increment = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_evo_increment), gtk_label_new("Increment: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_evo_increment), evo_gmenu->spin_evo_increment, TRUE, TRUE, 0);
	
    GtkWidget *hbox_evo_FPS = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_evo_FPS), gtk_label_new("FPS in movie: "), TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_evo_FPS), evo_gmenu->spin_evo_FPS, TRUE, TRUE, 0);

    GtkWidget *hbox_evo_fileformat = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_evo_fileformat), gtk_label_new("File format: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_evo_fileformat), evo_gmenu->combobox_evo_fileformat, TRUE, TRUE, 0);
	
	GtkWidget *hbox_evo_filename = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_evo_filename), gtk_label_new("Image file: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_evo_filename), evo_gmenu->entry_evo_file, TRUE, TRUE, 0);
	
	GtkWidget *hbox_evo_save = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_evo_save), evo_gmenu->evoView_Button, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_evo_save), evo_gmenu->evoSave_Button, TRUE, TRUE, 0);
	
	
    evo_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(evo_box), hbox_time, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(evo_box), hbox_fileindex, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(evo_box), hbox_evo_start, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(evo_box), hbox_evo_end, FALSE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(evo_box), hbox_evo_increment, FALSE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(evo_box), hbox_evo_FPS, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(evo_box), hbox_evo_filename, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(evo_box), hbox_evo_fileformat, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(evo_box), hbox_evo_save, FALSE, TRUE, 0);

	return evo_box;
}

GtkWidget * init_evolution_menu_frame(struct kemoviewer_type *kemo_sgl,
                                      struct evolution_gtk_menu *evo_gmenu,
                                      GtkWidget *evoWindow){
    set_evoluaiton_menu_expander(kemo_sgl, evoWindow, evo_gmenu);
    GtkWidget *evo_box = pack_evoluaiton_menu_box(evo_gmenu);
    return wrap_into_frame_gtk("Evolution", evo_box);
}

GtkWidget * init_evolution_menu_expander(struct kemoviewer_type *kemo_sgl,
                                         struct evolution_gtk_menu *evo_gmenu,
                                         GtkWidget *window){
    set_evoluaiton_menu_expander(kemo_sgl, window, evo_gmenu);
    GtkWidget *evo_box = pack_evoluaiton_menu_box(evo_gmenu);
    return wrap_into_scroll_expansion_gtk("Evolution", 360, 280, window, evo_box);
}

void activate_evolution_menu(struct kemoviewer_type *kemo_sgl,
                             GtkWidget *evo_widget){
    int iflag_draw_f = kemoview_get_fline_parameters(kemo_sgl, DRAW_SWITCH);
    int nload_psf = kemoview_get_PSF_loaded_params(kemo_sgl, NUM_LOADED);
    if(nload_psf > 0 || iflag_draw_f > 0){
        gtk_widget_set_sensitive(evo_widget, TRUE);
    }else{
        gtk_widget_set_sensitive(evo_widget, FALSE);
    }
    return;
}

