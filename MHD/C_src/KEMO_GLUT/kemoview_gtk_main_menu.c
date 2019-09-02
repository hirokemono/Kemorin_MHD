/*
 *  kemoview_gtk_main_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_main_menu.h"

GtkWidget *window_main;

int id_fmt = 0;

static void kemoview_update_CB(GtkButton *button, gpointer data){
	gtk_widget_destroy(window_main);
	gtk_main_quit();
	return;
};

static void set_viewtype_CB(GtkComboBox *combobox_viewtype, gpointer user_data)
{
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_viewtype);
    GtkTreeIter iter;
    cairo_t *cr;
    
    gchar *row_string;
    int index_field;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_viewtype);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_MATH, &index_mode, -1);
    
    printf("Selected mode %d, %s\n", index_mode, row_string);
	set_viewtype_mode_glut(index_mode);
//	draw_mesh_w_menu();
	return;
};

static void save_viewmatrix_CB(GtkButton *button, gpointer data){
	int iflag_set = kemoview_gtk_save_file_select(button, data);
	if(iflag_set == IZERO) return;
	
	GtkEntry *entry = GTK_ENTRY(data);
	struct kv_string *filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
	
	gtk_widget_destroy(window_main);
	gtk_main_quit();
	
	kemoview_write_modelview_file(filename);
	kemoview_free_kvstring(filename);
	
	return;
};
static void load_viewmatrix_CB(GtkButton *button, gpointer data){
	
	int iflag_set = kemoview_gtk_read_file_select(button, data);
	
	if(iflag_set == IZERO) return;
	gtk_widget_destroy(window_main);
	gtk_main_quit();
	
	GtkEntry *entry = GTK_ENTRY(data);
	struct kv_string *filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
	kemoview_load_modelview_file(filename);
	kemoview_free_kvstring(filename);
	
	return;
};

static void set_image_fileformat_CB(GtkComboBox *combobox_filefmt, gpointer user_data)
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
	id_fmt = index_mode;
	
//	draw_mesh_w_menu();
	return;
};

static void image_save_CB(GtkButton *button, gpointer data){
	int id_image;
	
	int iflag_set = kemoview_gtk_save_file_select(button, data);
	if(iflag_set == IZERO) return;
	
	GtkEntry *entry = GTK_ENTRY(data);
	struct kv_string *filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
    struct kv_string *stripped_ext = kemoview_alloc_kvstring();
	struct kv_string *file_prefix = kemoview_alloc_kvstring();
	
	gtk_widget_destroy(window_main);
	gtk_main_quit();
	
	
	kemoview_get_ext_from_file_name(filename, file_prefix, stripped_ext);
	id_image = kemoview_set_image_file_format_id(stripped_ext);
	if(id_image < 0) {
		id_image = id_fmt;
	};
	if(id_image == 0) return;
	kemoview_free_kvstring(filename);
	kemoview_free_kvstring(stripped_ext);
	
	printf("header: %s\n", file_prefix->string);
    kemoview_write_window_to_file(id_image, file_prefix);
    kemoview_free_kvstring(file_prefix);
	
	return;
};

static void open_file_CB(GtkButton *button, gpointer data){
	int iflag_datatype;
    struct kv_string *filename;
    struct kv_string *file_prefix;
    struct kv_string *stripped_ext;
    struct kv_string *command;
	
	int iflag_set = kemoview_gtk_read_file_select(button, data);
	if(iflag_set == IZERO) return;
	GtkEntry *entry = GTK_ENTRY(data);
	filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
	
	gtk_widget_destroy(window_main);
	gtk_main_quit();
	
    stripped_ext = kemoview_alloc_kvstring();
    file_prefix = kemoview_alloc_kvstring();
	iflag_datatype = kemoview_set_data_format_flag(filename, file_prefix, stripped_ext);
	printf("file name: %s\n", filename->string);
	printf("file_prefix %s\n", file_prefix->string);
	printf("stripped_ext %s\n", stripped_ext->string);
    kemoview_free_kvstring(stripped_ext);
    
    if(iflag_datatype == IFLAG_FULL_MESH_GZ || iflag_datatype == IFLAG_FULL_MESH){
        command = kemoview_alloc_kvstring();
        set_pickup_command_gtk(command);
        if(iflag_set == IZERO){
            kemoview_free_kvstring(file_prefix);
            kemoview_free_kvstring(filename);
            kemoview_free_kvstring(command);
            return;
        };
        kemoview_set_pick_surface_command(command);
        kemoview_free_kvstring(command);
        kemoview_free_kvstring(filename);
        
        filename = kemoview_alloc_kvstring();
        kemoview_alloc_kvstringitem(strlen(stripped_ext->string)+10, filename);
        strcpy(filename->string, file_prefix->string);
        strcat(filename->string, ".ksm");
        if(iflag_datatype == IFLAG_FULL_MESH_GZ){strcat(filename->string, ".gz");};
    };

	iflag_datatype = kemoview_open_data(filename);
    kemoview_free_kvstring(file_prefix);
    kemoview_free_kvstring(filename);
	return;
	return;
};

void gtk_main_menu(struct kemoviewer_type *kemoviewer_data){
	GtkWidget *hbox, *vbox;
	
	GtkWidget *entry;
	GtkWidget *updateButton;
	
	GtkWidget *hbox_open;
	GtkWidget *entry_file;
	GtkWidget *open_Button;
	
	GtkWidget *hbox_image_save;
	GtkWidget *entry_image_file;
	GtkWidget *imageSave_Button;
	
	GtkWidget *combobox_image_fileformat;
	GtkWidget *label_tree_image_fileformat;
	GtkCellRenderer *renderer_image_fileformat;
	GtkTreeModel *model_image_fileformat;
	GtkTreeModel *child_model_image_fileformat;
	
	GtkWidget *hbox_viewtype;
	GtkWidget *combobox_viewtype;
	GtkWidget *label_tree_viewtype;
	GtkCellRenderer *renderer_viewtype;
	GtkTreeModel *model_viewtype;
	GtkTreeModel *child_model_viewtype;
	
	GtkWidget *hbox_viewmatrix_save;
	GtkWidget *entry_viewmatrix_file;
	GtkWidget *saveView_Button, *loadView_Button;
	
	int index = 0;
	int iflag_mode;
	
	int iflag_draw_m = kemoview_get_draw_mesh_flag();
	int iflag_draw_p = kemoview_get_PSF_draw_switch();
	int iflag_draw_f = kemoview_get_fline_switch();
	int iflag_any_objects_on = iflag_draw_p + iflag_draw_m + iflag_draw_f;
	
	int nload_psf = kemoview_get_PSF_num_loaded();
	
	
	window_main = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	
	gtk_window_set_title(GTK_WINDOW(window_main), "Mesh viewer");
	gtk_widget_set_size_request(window_main, 150, -1);
	gtk_container_set_border_width(GTK_CONTAINER(window_main), 5);
	g_signal_connect(G_OBJECT(window_main), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	
		
	/* Set buttons   */
	entry = gtk_entry_new();
	updateButton = gtk_button_new_with_label("Update");
	g_signal_connect(G_OBJECT(updateButton), "clicked", 
				G_CALLBACK(kemoview_update_CB), (gpointer)entry);
	
	
	entry_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_file), "parent", (gpointer) window_main);
	open_Button = gtk_button_new_with_label("Open...");
	g_signal_connect(G_OBJECT(open_Button), "clicked", 
					 G_CALLBACK(open_file_CB), (gpointer)entry_file);
	
	entry_image_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_image_file), "parent", (gpointer) window_main);
	imageSave_Button = gtk_button_new_with_label("Save Image...");
	g_signal_connect(G_OBJECT(imageSave_Button), "clicked", 
					 G_CALLBACK(image_save_CB), (gpointer)entry_image_file);
	
	
	label_tree_viewtype = create_fixed_label_w_index_tree();
	model_viewtype = gtk_tree_view_get_model (label_tree_viewtype);  
	child_model_viewtype = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_viewtype));
	index = 0;
	index = append_ci_item_to_tree(index, "3D-Viewer", VIEW_3D, child_model_viewtype);
	index = append_ci_item_to_tree(index, "Stereo-Viewer", VIEW_STEREO, child_model_viewtype);
	index = append_ci_item_to_tree(index, "Map-Viewer", VIEW_MAP, child_model_viewtype);
	index = append_ci_item_to_tree(index, "XY-Viewer", VIEW_XY, child_model_viewtype);
	index = append_ci_item_to_tree(index, "XZ-Viewer", VIEW_XZ, child_model_viewtype);
	index = append_ci_item_to_tree(index, "YZ-Viewer", VIEW_YZ, child_model_viewtype);
	
	combobox_viewtype = gtk_combo_box_new_with_model(child_model_viewtype);
	renderer_viewtype = gtk_cell_renderer_text_new();
	iflag_mode = kemoview_get_view_type_flag();
	if(iflag_mode == VIEW_YZ){
		gtk_combo_box_set_active(combobox_viewtype, 5);
	}else if(iflag_mode == VIEW_XZ){
		gtk_combo_box_set_active(combobox_viewtype, 4);
	}else if(iflag_mode == VIEW_XY){
		gtk_combo_box_set_active(combobox_viewtype, 3);
	}else if(iflag_mode == VIEW_MAP){
		gtk_combo_box_set_active(combobox_viewtype, 2);
	}else if(iflag_mode == VIEW_STEREO){
		gtk_combo_box_set_active(combobox_viewtype, 1);
	} else {
		gtk_combo_box_set_active(combobox_viewtype, 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_viewtype), renderer_viewtype, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_viewtype), renderer_viewtype,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_viewtype), "changed", 
				G_CALLBACK(set_viewtype_CB), NULL);
	
	
	entry_viewmatrix_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_viewmatrix_file), "parent", (gpointer) window_main);
	saveView_Button = gtk_button_new_with_label("Save View...");
	g_signal_connect(G_OBJECT(saveView_Button), "clicked", 
					 G_CALLBACK(save_viewmatrix_CB), (gpointer)entry_viewmatrix_file);
	loadView_Button = gtk_button_new_with_label("Load View...");
	g_signal_connect(G_OBJECT(loadView_Button), "clicked", 
					 G_CALLBACK(load_viewmatrix_CB), (gpointer)entry_viewmatrix_file);
	
	
	
	label_tree_image_fileformat = create_fixed_label_w_index_tree();
	model_image_fileformat = gtk_tree_view_get_model (label_tree_image_fileformat);  
	child_model_image_fileformat = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_image_fileformat));
	index = 0;
	index = append_ci_item_to_tree(index, "No Image", NO_SAVE_FILE, child_model_image_fileformat);
	index = append_ci_item_to_tree(index, "PNG", SAVE_PNG, child_model_image_fileformat);
	index = append_ci_item_to_tree(index, "BMP", SAVE_BMP, child_model_image_fileformat);
	
	combobox_image_fileformat = gtk_combo_box_new_with_model(child_model_image_fileformat);
	renderer_image_fileformat = gtk_cell_renderer_text_new();
	id_fmt = NO_SAVE_FILE;
	if(id_fmt == SAVE_BMP){
		gtk_combo_box_set_active(combobox_image_fileformat, 2);
	} else if(id_fmt == SAVE_PNG){
		gtk_combo_box_set_active(combobox_image_fileformat, 1);
	} else {
		gtk_combo_box_set_active(combobox_image_fileformat, 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_image_fileformat), renderer_image_fileformat, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_image_fileformat), renderer_image_fileformat,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_image_fileformat), "changed", 
				G_CALLBACK(set_image_fileformat_CB), NULL);
	
	
	hbox_open = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_open), gtk_label_new("File: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_open), entry_file, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_open), open_Button, FALSE, FALSE, 0);
	
	hbox_image_save = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_image_save), gtk_label_new("Image file: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_image_save), combobox_image_fileformat, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_image_save), imageSave_Button, FALSE, FALSE, 0);
	
	hbox_viewtype = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_viewtype), gtk_label_new("View type: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_viewtype), combobox_viewtype, FALSE, FALSE, 0);
	
	hbox_viewmatrix_save = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_viewmatrix_save), saveView_Button, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_viewmatrix_save), loadView_Button, FALSE, FALSE, 0);
	
	
	vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_container_add(GTK_CONTAINER(window_main), vbox);
	
	gtk_box_pack_start(GTK_BOX(vbox), hbox_open, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_image_save, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox), hbox_viewtype, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_viewmatrix_save, FALSE, FALSE, 0);
	
	add_axis_menu_box(kemoviewer_data, vbox);
	add_rotation_menu_box(kemoviewer_data, window_main, vbox);
	if(nload_psf > 0 || iflag_draw_f){
		add_evoluaiton_menu_box(kemoviewer_data, window_main, vbox);
	};
	gtk_box_pack_start(GTK_BOX(vbox), updateButton, FALSE, FALSE, 0);
	
	
	gtk_widget_show_all(window_main);
	gtk_main();
	return;
}
