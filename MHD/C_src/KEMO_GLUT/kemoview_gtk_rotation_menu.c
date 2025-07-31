/*
 *  kemoview_gtk_rotation_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_rotation_menu.h"

#define X_AXIS 1
#define Y_AXIS 2
#define Z_AXIS 3

struct rotation_gtk_menu * init_rotation_menu_box(void){
	struct rotation_gtk_menu *rot_gmenu
			= (struct rotation_gtk_menu *)  malloc(sizeof(struct rotation_gtk_menu));
	rot_gmenu->id_fmt_rot = 0;
	
	rot_gmenu->inc_deg = 2;
	rot_gmenu->iaxis_rot = Z_AXIS;
	return rot_gmenu;
};

static void set_rotation_direction_CB(GtkComboBox *combobox_rotdir, gpointer user_data)
{
	struct rotation_gtk_menu *rot_gmenu 
			= (struct rotation_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "rotation");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
    
    rot_gmenu->iaxis_rot = gtk_selected_combobox_index(combobox_rotdir);
	draw_full_gl(kemo_gl);
	return;
};

static void set_rotation_fileformat_CB(GtkComboBox *combobox_filefmt, gpointer user_data)
{
	struct rotation_gtk_menu *rot_gmenu 
			= (struct rotation_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "rotation");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
    
    rot_gmenu->id_fmt_rot = gtk_selected_combobox_index(combobox_filefmt);
    draw_full_gl(kemo_gl);
	return;
};

static void rotation_FPS_CB(GtkWidget *entry, gpointer user_data)
{
    struct rotation_gtk_menu *rot_gmenu
            = (struct rotation_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "rotation");
     rot_gmenu->i_FPS = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
/*    printf("FPS %d\n", rot_gmenu->i_FPS);*/
}

static void rotation_increment_CB(GtkWidget *entry, gpointer user_data)
{
	struct rotation_gtk_menu *rot_gmenu 
			= (struct rotation_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "rotation");
	 rot_gmenu->inc_deg = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
/*	printf("radius %d\n", radius);*/
}

static void rotation_view_CB(GtkButton *button, gpointer user_data){
	GtkEntry *entry = GTK_ENTRY(user_data);
	GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct rotation_gtk_menu *rot_gmenu 
			= (struct rotation_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "rotation");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
	
	gtk_window_set_focus(GTK_WINDOW(window), NULL);
    draw_rotate_views(kemo_gl, rot_gmenu->iaxis_rot, rot_gmenu->inc_deg, IONE);
	return;
};

static void rotation_save_CB(GtkButton *button, gpointer user_data){
	GtkEntry *entry = GTK_ENTRY(user_data);
	GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct rotation_gtk_menu *rot_gmenu 
			= (struct rotation_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "rotation");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");

	int id_image;
    kemoview_gtk_save_file_select(button, G_OBJECT(entry));
	struct kv_string *filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
    struct kv_string *stripped_ext = kemoview_alloc_kvstring();
	struct kv_string *file_prefix = kemoview_alloc_kvstring();
	
	kemoview_get_ext_from_file_name(filename, file_prefix, stripped_ext);
	id_image = kemoview_set_image_file_format_id(stripped_ext);
	if(id_image < 0) {
		id_image = rot_gmenu->id_fmt_rot;
	};
	if(id_image == 0) return;
	kemoview_free_kvstring(stripped_ext);
	kemoview_free_kvstring(filename);
	
	gtk_window_set_focus(GTK_WINDOW(window), NULL);
    sel_write_rotate_views(kemo_gl, rot_gmenu->id_fmt_rot, file_prefix,
                           rot_gmenu->i_FPS, rot_gmenu->iaxis_rot, rot_gmenu->inc_deg);
	
	return;
};


GtkWidget * init_rotation_menu_expander(struct kemoviewer_gl_type *kemo_gl,
                                        struct rotation_gtk_menu *rot_gmenu,
                                        GtkWidget *window){
	GtkWidget *expander_rot;
	
	GtkWidget *entry_rotation_file = gtk_entry_new();
    g_object_set_data(G_OBJECT(entry_rotation_file), "kemoview_gl",  (gpointer) kemo_gl);
    g_object_set_data(G_OBJECT(entry_rotation_file), "parent", (gpointer) window);
	g_object_set_data(G_OBJECT(entry_rotation_file), "rotation", (gpointer) rot_gmenu);

	GtkWidget *label_tree_rotation_dir = create_fixed_label_w_index_tree();
	GtkTreeModel *model_rotation_dir = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_rotation_dir));  
	GtkTreeModel *child_model_rotation_dir = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_rotation_dir));
	int index = 0;
	index = append_ci_item_to_tree(index, "X-axis", X_AXIS, child_model_rotation_dir);
	index = append_ci_item_to_tree(index, "Y-axis", Y_AXIS, child_model_rotation_dir);
	index = append_ci_item_to_tree(index, "Z-axis", Z_AXIS, child_model_rotation_dir);
	
	rot_gmenu->combobox_rotation_dir = gtk_combo_box_new_with_model(child_model_rotation_dir);
	GtkCellRenderer *renderer_rotation_dir = gtk_cell_renderer_text_new();
	if(rot_gmenu->iaxis_rot == Z_AXIS){
		gtk_combo_box_set_active(GTK_COMBO_BOX(rot_gmenu->combobox_rotation_dir), 2);
	} else if(rot_gmenu->iaxis_rot == Y_AXIS){
		gtk_combo_box_set_active(GTK_COMBO_BOX(rot_gmenu->combobox_rotation_dir), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(rot_gmenu->combobox_rotation_dir), 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(rot_gmenu->combobox_rotation_dir),
							   renderer_rotation_dir, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(rot_gmenu->combobox_rotation_dir), 
								   renderer_rotation_dir, "text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(rot_gmenu->combobox_rotation_dir), "changed",
				G_CALLBACK(set_rotation_direction_CB), entry_rotation_file);
	
	
	GtkWidget *label_tree_rotation_fileformat = create_fixed_label_w_index_tree();
	GtkTreeModel *model_rotation_fileformat = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_rotation_fileformat));  
	GtkTreeModel *child_model_rotation_fileformat = 
			gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_rotation_fileformat));
	index = 0;
	index = append_ci_item_to_tree(index, "No Image", NO_SAVE_FILE,  child_model_rotation_fileformat);
	index = append_ci_item_to_tree(index, "PNG",      SAVE_PNG,      child_model_rotation_fileformat);
	index = append_ci_item_to_tree(index, "BMP",      SAVE_BMP,      child_model_rotation_fileformat);
#ifdef FFMPEG
    index = append_ci_item_to_tree(index, "Movie",    SAVE_QT_MOVIE, child_model_rotation_fileformat);
#endif

	rot_gmenu->combobox_rotation_fileformat = 
			gtk_combo_box_new_with_model(child_model_rotation_fileformat);
	GtkCellRenderer *renderer_rotation_fileformat = gtk_cell_renderer_text_new();
	rot_gmenu->id_fmt_rot = NO_SAVE_FILE;
	if(rot_gmenu->id_fmt_rot == SAVE_BMP){
		gtk_combo_box_set_active(GTK_COMBO_BOX(rot_gmenu->combobox_rotation_fileformat), 2);
	} else if(rot_gmenu->id_fmt_rot == SAVE_PNG){
		gtk_combo_box_set_active(GTK_COMBO_BOX(rot_gmenu->combobox_rotation_fileformat), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(rot_gmenu->combobox_rotation_fileformat), 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(rot_gmenu->combobox_rotation_fileformat), 
							   renderer_rotation_fileformat, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(rot_gmenu->combobox_rotation_fileformat), 
								   renderer_rotation_fileformat, "text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(rot_gmenu->combobox_rotation_fileformat), "changed",
                     G_CALLBACK(set_rotation_fileformat_CB), entry_rotation_file);
	
	
    rot_gmenu->i_FPS = 30;
	GtkAdjustment *adj_rot_FPS = gtk_adjustment_new(rot_gmenu->i_FPS, 1, 180, 1, 1, 0.0);
	rot_gmenu->spin_rot_FPS = gtk_spin_button_new(GTK_ADJUSTMENT(adj_rot_FPS), 0, 1);
    gtk_spin_button_set_digits(GTK_SPIN_BUTTON(rot_gmenu->spin_rot_FPS), 0);
    gtk_entry_set_width_chars(GTK_ENTRY(rot_gmenu->spin_rot_FPS), 6);
	g_signal_connect(rot_gmenu->spin_rot_FPS, "value-changed",
					 G_CALLBACK(rotation_FPS_CB),entry_rotation_file);
		
    GtkAdjustment *adj_rot_increment = gtk_adjustment_new(rot_gmenu->inc_deg, 0.0, 180.0, 1, 1, 0.0);
    rot_gmenu->spin_rot_increment = gtk_spin_button_new(GTK_ADJUSTMENT(adj_rot_increment), 0, 1);
    gtk_spin_button_set_digits(GTK_SPIN_BUTTON(rot_gmenu->spin_rot_increment), 0);
    gtk_entry_set_width_chars(GTK_ENTRY(rot_gmenu->spin_rot_increment), 6);
    g_signal_connect(rot_gmenu->spin_rot_increment, "value-changed",
                     G_CALLBACK(rotation_increment_CB),entry_rotation_file);
        
	rot_gmenu->rotView_Button = gtk_button_new_with_label("View Rotation");
	g_signal_connect(G_OBJECT(rot_gmenu->rotView_Button), "clicked", 
					 G_CALLBACK(rotation_view_CB), (gpointer)entry_rotation_file);
	rot_gmenu->rotSave_Button = gtk_button_new_with_label("Save Rotation");
	g_signal_connect(G_OBJECT(rot_gmenu->rotSave_Button), "clicked", 
					 G_CALLBACK(rotation_save_CB), (gpointer)entry_rotation_file);
	
	
	GtkWidget *hbox_rotation_dir = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_dir), gtk_label_new("Rotation axis: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_dir), rot_gmenu->combobox_rotation_dir, TRUE, TRUE, 0);
	
	GtkWidget *hbox_rot_increment = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_rot_increment), gtk_label_new("Step (Deg.): "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_rot_increment), rot_gmenu->spin_rot_increment, FALSE, FALSE, 0);
	
    GtkWidget *hbox_rot_FPS = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_box_pack_start(GTK_BOX(hbox_rot_FPS), gtk_label_new("FPS for movie: "), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_rot_FPS), rot_gmenu->spin_rot_FPS, FALSE, FALSE, 0);
    	
	GtkWidget *hbox_rotation_fileformat = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_fileformat), gtk_label_new("File format: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_fileformat), rot_gmenu->combobox_rotation_fileformat,
					   TRUE, TRUE, 0);
	
	GtkWidget *hbox_rotation_save = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_save), rot_gmenu->rotView_Button, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_save), rot_gmenu->rotSave_Button, TRUE, TRUE, 0);
	
	
    GtkWidget *rot_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(rot_box), hbox_rotation_dir, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(rot_box), hbox_rot_increment, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(rot_box), hbox_rot_FPS, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(rot_box), hbox_rotation_fileformat, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(rot_box), hbox_rotation_save, FALSE, FALSE, 0);
	
	expander_rot = wrap_into_scroll_expansion_gtk("Rotation", 360, 240, window, rot_box);
	return expander_rot;
}
