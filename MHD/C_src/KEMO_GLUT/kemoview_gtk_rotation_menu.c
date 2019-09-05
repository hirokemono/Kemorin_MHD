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

int iaxis_rot = Z_AXIS;
int inc_deg = 2;

int id_fmt_rot = 0;

static void set_rotation_direction_CB(GtkComboBox *combobox_rotdir, gpointer user_data)
{
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_rotdir);
    GtkTreeIter iter;
    cairo_t *cr;
    
    gchar *row_string;
    int index_field;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_rotdir);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_MATH, &index_mode, -1);
    
    printf("Selected mode %d, %s\n", index_mode, row_string);
	iaxis_rot = index_mode;
	
//	draw_mesh_w_menu();
	return;
};

static void set_rotation_fileformat_CB(GtkComboBox *combobox_filefmt, gpointer user_data)
{
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_filefmt);
    GtkTreeIter iter;
    cairo_t *cr;
    
    gchar *row_string;
    int index_field;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_filefmt);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_MATH, &index_mode, -1);
    
    printf("Selected mode %d, %s\n", index_mode, row_string);
	id_fmt_rot = index_mode;
	
//	draw_mesh_w_menu();
	return;
};

static void rotation_increment_CB(GtkWidget *entry, gpointer user_data)
{
	 inc_deg = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
/*	printf("radius %d\n", radius);*/
}

static void rotation_view_CB(GtkButton *button, gpointer user_data){
	GtkEntry *entry = GTK_ENTRY(user_data);
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	gtk_widget_destroy(window_main);
	gtk_main_quit();
	
	struct kv_string *image_prefix = kemoview_init_kvstring_by_string("Kemoviewer");
	
	gtk_window_set_focus(GTK_WINDOW(window_main), NULL);
	write_rotate_views_glut(NO_SAVE_FILE, image_prefix, iaxis_rot, inc_deg);
	kemoview_free_kvstring(image_prefix);
	return;
};
static void rotation_save_CB(GtkButton *button, gpointer user_data){
	GtkEntry *entry = GTK_ENTRY(user_data);
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	int id_image;
	struct kv_string *filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
    struct kv_string *stripped_ext = kemoview_alloc_kvstring();
	struct kv_string *file_prefix = kemoview_alloc_kvstring();
	
	gtk_widget_destroy(window_main);
	gtk_main_quit();
	
	
	kemoview_get_ext_from_file_name(filename, file_prefix, stripped_ext);
	id_image = kemoview_set_image_file_format_id(stripped_ext);
	if(id_image < 0) {
		id_image = id_fmt_rot;
	};
	if(id_image == 0) return;
	kemoview_free_kvstring(stripped_ext);
	kemoview_free_kvstring(filename);
	
	gtk_window_set_focus(GTK_WINDOW(window_main), NULL);
	write_rotate_views_glut(id_fmt_rot, file_prefix, iaxis_rot, inc_deg);
	
	return;
};


void add_rotation_menu_box(struct kemoviewer_type *kemoviewer_data, 
			GtkWidget *window_main, GtkWidget *box_out){
	GtkWidget *vbox;
	
	GtkWidget *hbox_rotation_dir;
	GtkWidget *combobox_rotation_dir;
	GtkWidget *label_tree_rotation_dir;
	GtkCellRenderer *renderer_rotation_dir;
	GtkTreeModel *model_rotation_dir;
	GtkTreeModel *child_model_rotation_dir;
	
	GtkWidget *hbox_rot_increment, *hbox_org_rot_increment;
	GtkWidget *spin_rot_increment;
	GtkAdjustment *adj_rot_increment;
	int current_rot_increment;
	char current_rot_inc_text[30];
	
	GtkWidget *hbox_rotation_fileformat;
	GtkWidget *combobox_rotation_fileformat;
	GtkWidget *label_tree_rotation_fileformat;
	GtkCellRenderer *renderer_rotation_fileformat;
	GtkTreeModel *model_rotation_fileformat;
	GtkTreeModel *child_model_rotation_fileformat;
	
	GtkWidget *hbox_rotation_filename;
	GtkWidget *entry_rotation_file;
	GtkWidget *rotSelect_Button;
	
	GtkWidget *hbox_rotation_save;
	GtkWidget *rotView_Button;
	GtkWidget *rotSave_Button;
	
	int index = 0;
	
	label_tree_rotation_dir = create_fixed_label_w_index_tree();
	model_rotation_dir = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_rotation_dir));  
	child_model_rotation_dir = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_rotation_dir));
	index = 0;
	index = append_ci_item_to_tree(index, "X-axis", X_AXIS, child_model_rotation_dir);
	index = append_ci_item_to_tree(index, "Y-axis", Y_AXIS, child_model_rotation_dir);
	index = append_ci_item_to_tree(index, "Z-axis", Z_AXIS, child_model_rotation_dir);
	
	combobox_rotation_dir = gtk_combo_box_new_with_model(child_model_rotation_dir);
	renderer_rotation_dir = gtk_cell_renderer_text_new();
	if(iaxis_rot == Z_AXIS){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_rotation_dir), 2);
	} else if(iaxis_rot == Y_AXIS){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_rotation_dir), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_rotation_dir), 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_rotation_dir), renderer_rotation_dir, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_rotation_dir), renderer_rotation_dir,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_rotation_dir), "changed", 
				G_CALLBACK(set_rotation_direction_CB), NULL);
	
	
	label_tree_rotation_fileformat = create_fixed_label_w_index_tree();
	model_rotation_fileformat = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_rotation_fileformat));  
	child_model_rotation_fileformat = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_rotation_fileformat));
	index = 0;
	index = append_ci_item_to_tree(index, "No Image", NO_SAVE_FILE, child_model_rotation_fileformat);
	index = append_ci_item_to_tree(index, "PNG", SAVE_PNG, child_model_rotation_fileformat);
	index = append_ci_item_to_tree(index, "BMP", SAVE_BMP, child_model_rotation_fileformat);
	
	combobox_rotation_fileformat = gtk_combo_box_new_with_model(child_model_rotation_fileformat);
	renderer_rotation_fileformat = gtk_cell_renderer_text_new();
	id_fmt_rot = NO_SAVE_FILE;
	if(id_fmt_rot == SAVE_BMP){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_rotation_fileformat), 2);
	} else if(id_fmt_rot == SAVE_PNG){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_rotation_fileformat), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_rotation_fileformat), 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_rotation_fileformat), renderer_rotation_fileformat, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_rotation_fileformat), renderer_rotation_fileformat,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_rotation_fileformat), "changed", 
				G_CALLBACK(set_rotation_fileformat_CB), NULL);
	
	
	
	current_rot_increment = inc_deg;
	sprintf(current_rot_inc_text, "    %d    ", current_rot_increment);
	adj_rot_increment = gtk_adjustment_new(current_rot_increment, 0.0, 180.0, 1, 1, 0.0);
	spin_rot_increment = gtk_spin_button_new(GTK_ADJUSTMENT(adj_rot_increment), 0, 3);
	g_signal_connect(spin_rot_increment, "value-changed", G_CALLBACK(rotation_increment_CB),NULL);
	
	entry_rotation_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_rotation_file), "parent", (gpointer) window_main);
	
	rotSelect_Button = gtk_button_new_with_label("Select...");
	g_signal_connect(rotSelect_Button, "clicked", G_CALLBACK(kemoview_gtk_save_file_select),
				(gpointer) entry_rotation_file);
	
	
	rotView_Button = gtk_button_new_with_label("View Rotation");
	g_signal_connect(G_OBJECT(rotView_Button), "clicked", 
					 G_CALLBACK(rotation_view_CB), (gpointer)entry_rotation_file);
	rotSave_Button = gtk_button_new_with_label("Save Rotation");
	g_signal_connect(G_OBJECT(rotSave_Button), "clicked", 
					 G_CALLBACK(rotation_save_CB), (gpointer)entry_rotation_file);
	
	
	hbox_rotation_dir = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_dir), gtk_label_new("Surface direction: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_dir), combobox_rotation_dir, TRUE, TRUE, 0);
	
	hbox_org_rot_increment = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_org_rot_increment), gtk_label_new("Current increment: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_org_rot_increment), gtk_label_new(current_rot_inc_text), TRUE, TRUE, 0);
	hbox_rot_increment = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_rot_increment), gtk_label_new("Step (Deg.): "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_rot_increment), spin_rot_increment, TRUE, TRUE, 0);
	
	hbox_rotation_filename = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_filename), gtk_label_new("Image file: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_filename), entry_rotation_file, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_filename), rotSelect_Button, TRUE, TRUE, 0);
	
	hbox_rotation_fileformat = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_fileformat), gtk_label_new("File format: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_fileformat), combobox_rotation_fileformat, TRUE, TRUE, 0);
	
	hbox_rotation_save = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_save), rotView_Button, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_save), rotSave_Button, TRUE, TRUE, 0);
	
	
	vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_rotation_dir, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_org_rot_increment, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_rot_increment, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_rotation_filename, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_rotation_fileformat, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_rotation_save, FALSE, TRUE, 0);
	
	wrap_into_expanded_frame_gtk("Rotation", vbox, box_out);
	return;
}
