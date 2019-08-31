/*
 *  kemoview_gtk_main_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_main_menu.h"

#define X_AXIS 1
#define Y_AXIS 2
#define Z_AXIS 3

GtkWidget *window_main;

int iaxis_rot = Z_AXIS;
int inc_deg = 2;

int id_fmt = 0;

int istart_evo = 0;
int iend_evo = 0;
int inc_evo = 1;


static void kemoview_update_CB(GtkButton *button, gpointer data){
	gtk_widget_destroy(window_main);
	gtk_main_quit();
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
	
	int iflag_set = kemoview_gtk_read_file_select(data);
	
	if(iflag_set == IZERO) return;
	gtk_widget_destroy(window_main);
	gtk_main_quit();
	
	GtkEntry *entry = GTK_ENTRY(data);
	struct kv_string *filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
	kemoview_load_modelview_file(filename);
	kemoview_free_kvstring(filename);
	
	return;
};

static void draw_axis_switch_CB(GObject *switch_bar, GParamSpec *pspec, gpointer data){
	int toggle = kemoview_toggle_object_properties(AXIS_TOGGLE);
	return;
};
static void draw_coastline_switch_CB(GObject *switch_bar, GParamSpec *pspec, gpointer data){
	int toggle = kemoview_toggle_object_properties(COASTLINE_SWITCH);
	return;
};
static void draw_sph_grid_switch_CB(GObject *switch_bar, GParamSpec *pspec, gpointer data){
	int toggle = kemoview_toggle_object_properties(SPHEREGRID_SWITCH);
	return;
};
static void coastline_radius_CB(GtkWidget *entry, gpointer data)
{
	double radius = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_coastline_radius(radius);
/*	printf("radius %d\n", radius);*/
}

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

static void set_shading_mode_CB(GtkComboBox *combobox_shading, gpointer user_data)
{
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_shading);
    GtkTreeIter iter;
    cairo_t *cr;
    
    gchar *row_string;
    int index_field;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_shading);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_MATH, &index_mode, -1);
    
    printf("Selected mode %d, %s\n", index_mode, row_string);
	kemoview_set_object_property_flags(SHADING_SWITCH, index_mode);
	
//	draw_mesh_w_menu();
	return;
};

static void set_surface_direction_CB(GtkComboBox *combobox_surfdir, gpointer user_data)
{
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_surfdir);
    GtkTreeIter iter;
    cairo_t *cr;
    
    gchar *row_string;
    int index_field;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_surfdir);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_MATH, &index_mode, -1);
    
    printf("Selected mode %d, %s\n", index_mode, row_string);
	kemoview_set_object_property_flags(POLYGON_SWITCH, index_mode);
	
//	draw_mesh_w_menu();
	return;
};

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
	id_fmt = index_mode;
	
//	draw_mesh_w_menu();
	return;
};

static void rotation_increment_CB(GtkWidget *entry, gpointer data)
{
	 inc_deg = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
/*	printf("radius %d\n", radius);*/
}

static void rotation_view_CB(GtkButton *button, gpointer data){
	gtk_widget_destroy(window_main);
	gtk_main_quit();
	
	struct kv_string *image_prefix = kemoview_init_kvstring_by_string("Kemoviewer");
	
	write_rotate_views_glut(NO_SAVE_FILE, image_prefix, iaxis_rot, inc_deg);
	kemoview_free_kvstring(image_prefix);
	return;
};
static void rotation_save_CB(GtkButton *button, gpointer data){
	int id_image;
	GtkEntry *entry = GTK_ENTRY(data);
	struct kv_string *filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
    struct kv_string *stripped_ext;
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
	
	write_rotate_views_glut(id_fmt, file_prefix, iaxis_rot, inc_deg);
	
	return;
};

static void evolution_view_CB(GtkButton *button, gpointer data){
	gtk_widget_destroy(window_main);
	gtk_main_quit();
	
	struct kv_string *image_prefix = kemoview_init_kvstring_by_string("Kemoviewer");
	
	write_evolution_views_glut(NO_SAVE_FILE, image_prefix, istart_evo, iend_evo, inc_evo);
	kemoview_free_kvstring(image_prefix);
	return;
};
static void evolution_save_CB(GtkButton *button, gpointer data){
	int id_image;
	GtkEntry *entry = GTK_ENTRY(data);
	struct kv_string *filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
    struct kv_string *stripped_ext;
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
	printf("steps: %d %d %d\n", istart_evo, iend_evo, inc_evo);
	write_evolution_views_glut(id_image, file_prefix, istart_evo, iend_evo, inc_evo);
    kemoview_free_kvstring(file_prefix);
	
	return;
};

static void evo_start_step_CB(GtkWidget *entry, gpointer data)
{
	 istart_evo = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
/*	printf("radius %d\n", radius);*/
}
static void evo_end_step_CB(GtkWidget *entry, gpointer data)
{
	 iend_evo = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
/*	printf("radius %d\n", radius);*/
}
static void evo_increment_CB(GtkWidget *entry, gpointer data)
{
	 inc_evo = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
/*	printf("radius %d\n", radius);*/
}

static void set_evo_fileformat_CB(GtkComboBox *combobox_filefmt, gpointer user_data)
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

void gtk_main_menu(struct kemoviewer_type *kemoviewer_data){
	GtkWidget *hbox, *vbox;
	
	GtkWidget *entry;
	GtkWidget *updateButton;
	
	GtkWidget *hbox_axis, *hbox_sph_grid, *hbox_coastline;
	GtkWidget *switch_axis, *switch_sph_grid, *switch_coastline;
	
	GtkWidget *hbox_coast_radius, *hbox_org_radius;
	GtkWidget *spin_coast_radius;
	GtkAdjustment *adj_coast_radius;
	double current_radius;
	char current_radius_text[30];
	
	GtkWidget *hbox_viewtype;
	GtkWidget *combobox_viewtype;
	GtkWidget *label_tree_viewtype;
	GtkCellRenderer *renderer_viewtype;
	GtkTreeModel *model_viewtype;
	GtkTreeModel *child_model_viewtype;
	
	GtkWidget *hbox_viewmatrix_save;
	GtkWidget *entry_viewmatrix_file;
	GtkWidget *saveView_Button, *loadView_Button;
	
	GtkWidget *hbox_shading;
	GtkWidget *combobox_shading;
	GtkWidget *label_tree_shading;
	GtkCellRenderer *renderer_shading;
	GtkTreeModel *model_shading;
	GtkTreeModel *child_model_shading;
	
	GtkWidget *hbox_surf_dir;
	GtkWidget *combobox_surf_dir;
	GtkWidget *label_tree_surf_dir;
	GtkCellRenderer *renderer_surf_dir;
	GtkTreeModel *model_surf_dir;
	GtkTreeModel *child_model_surf_dir;
	
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
	
	GtkWidget *hbox_evo_start;
	GtkWidget *spin_evo_start;
	GtkAdjustment *adj_evo_start;
	int current_evo_start;
	char current_evo_start_text[30];
	
	GtkWidget *hbox_evo_end;
	GtkWidget *spin_evo_end;
	GtkAdjustment *adj_evo_end;
	int current_evo_end;
	char current_evo_end_text[30];
	
	GtkWidget *hbox_evo_increment;
	GtkWidget *spin_evo_increment;
	GtkAdjustment *adj_evo_increment;
	int current_evo_increment;
	char current_evo_inc_text[30];
	
	GtkWidget *hbox_evo_fileformat;
	GtkWidget *combobox_evo_fileformat;
	GtkWidget *label_tree_evo_fileformat;
	GtkCellRenderer *renderer_evo_fileformat;
	GtkTreeModel *model_evo_fileformat;
	GtkTreeModel *child_model_evo_fileformat;
	
	GtkWidget *hbox_evo_filename;
	GtkWidget *entry_evo_file;
	GtkWidget *evoSelect_Button;
	
	GtkWidget *hbox_evo_save;
	GtkWidget *evoView_Button;
	GtkWidget *evoSave_Button;
	
	int index = 0;
	int iflag_mode;
	
	
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
	
	
	
	label_tree_shading = create_fixed_label_w_index_tree();
	model_shading = gtk_tree_view_get_model (label_tree_shading);  
	child_model_shading = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_shading));
	index = 0;
	index = append_ci_item_to_tree(index, "Smooth surface", SMOOTH_SHADE, child_model_shading);
	index = append_ci_item_to_tree(index, "Flat surface", FLAT_SHADE, child_model_shading);
	
	combobox_shading = gtk_combo_box_new_with_model(child_model_shading);
	renderer_shading = gtk_cell_renderer_text_new();
	iflag_mode = kemoview_get_object_property_flags(SHADING_SWITCH);
	if(iflag_mode == FLAT_SHADE){
		gtk_combo_box_set_active(combobox_shading, 1);
	} else {
		gtk_combo_box_set_active(combobox_shading, 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_shading), renderer_shading, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_shading), renderer_shading,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_shading), "changed", 
				G_CALLBACK(set_shading_mode_CB), NULL);
	
	
	label_tree_surf_dir = create_fixed_label_w_index_tree();
	model_surf_dir = gtk_tree_view_get_model (label_tree_surf_dir);  
	child_model_surf_dir = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_surf_dir));
	index = 0;
	index = append_ci_item_to_tree(index, "Normal surface", NORMAL_POLYGON, child_model_surf_dir);
	index = append_ci_item_to_tree(index, "Reverse surface", REVERSE_POLYGON, child_model_surf_dir);
	
	combobox_surf_dir = gtk_combo_box_new_with_model(child_model_surf_dir);
	renderer_surf_dir = gtk_cell_renderer_text_new();
	iflag_mode = kemoview_get_object_property_flags(POLYGON_SWITCH);
	if(iflag_mode == REVERSE_POLYGON){
		gtk_combo_box_set_active(combobox_surf_dir, 1);
	} else {
		gtk_combo_box_set_active(combobox_surf_dir, 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_surf_dir), renderer_surf_dir, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_surf_dir), renderer_surf_dir,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_surf_dir), "changed", 
				G_CALLBACK(set_surface_direction_CB), NULL);
	
	
	label_tree_rotation_dir = create_fixed_label_w_index_tree();
	model_rotation_dir = gtk_tree_view_get_model (label_tree_rotation_dir);  
	child_model_rotation_dir = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_rotation_dir));
	index = 0;
	index = append_ci_item_to_tree(index, "X-axis", X_AXIS, child_model_rotation_dir);
	index = append_ci_item_to_tree(index, "Y-axis", Y_AXIS, child_model_rotation_dir);
	index = append_ci_item_to_tree(index, "Z-axis", Z_AXIS, child_model_rotation_dir);
	
	combobox_rotation_dir = gtk_combo_box_new_with_model(child_model_rotation_dir);
	renderer_rotation_dir = gtk_cell_renderer_text_new();
	if(iaxis_rot == Z_AXIS){
		gtk_combo_box_set_active(combobox_rotation_dir, 2);
	} else if(iaxis_rot == Y_AXIS){
		gtk_combo_box_set_active(combobox_rotation_dir, 1);
	} else {
		gtk_combo_box_set_active(combobox_rotation_dir, 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_rotation_dir), renderer_rotation_dir, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_rotation_dir), renderer_rotation_dir,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_rotation_dir), "changed", 
				G_CALLBACK(set_rotation_direction_CB), NULL);
	
	
	label_tree_evo_fileformat = create_fixed_label_w_index_tree();
	model_rotation_fileformat = gtk_tree_view_get_model (label_tree_evo_fileformat);  
	child_model_rotation_fileformat = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_rotation_fileformat));
	index = 0;
	index = append_ci_item_to_tree(index, "No Image", NO_SAVE_FILE, child_model_rotation_fileformat);
	index = append_ci_item_to_tree(index, "PNG", SAVE_PNG, child_model_rotation_fileformat);
	index = append_ci_item_to_tree(index, "BMP", SAVE_BMP, child_model_rotation_fileformat);
	
	combobox_rotation_fileformat = gtk_combo_box_new_with_model(child_model_rotation_fileformat);
	renderer_rotation_fileformat = gtk_cell_renderer_text_new();
	id_fmt = NO_SAVE_FILE;
	if(id_fmt == SAVE_BMP){
		gtk_combo_box_set_active(combobox_rotation_fileformat, 2);
	} else if(id_fmt == SAVE_PNG){
		gtk_combo_box_set_active(combobox_rotation_fileformat, 1);
	} else {
		gtk_combo_box_set_active(combobox_rotation_fileformat, 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_rotation_fileformat), renderer_rotation_fileformat, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_rotation_fileformat), renderer_rotation_fileformat,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_rotation_fileformat), "changed", 
				G_CALLBACK(set_rotation_fileformat_CB), NULL);
	
	
	label_tree_evo_fileformat = create_fixed_label_w_index_tree();
	model_evo_fileformat = gtk_tree_view_get_model (label_tree_evo_fileformat);  
	child_model_evo_fileformat = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_evo_fileformat));
	index = 0;
	index = append_ci_item_to_tree(index, "No Image", NO_SAVE_FILE, child_model_evo_fileformat);
	index = append_ci_item_to_tree(index, "PNG", SAVE_PNG, child_model_evo_fileformat);
	index = append_ci_item_to_tree(index, "BMP", SAVE_BMP, child_model_evo_fileformat);
	
	combobox_evo_fileformat = gtk_combo_box_new_with_model(child_model_evo_fileformat);
	renderer_evo_fileformat = gtk_cell_renderer_text_new();
	id_fmt = NO_SAVE_FILE;
	if(id_fmt == SAVE_BMP){
		gtk_combo_box_set_active(combobox_evo_fileformat, 2);
	} else if(id_fmt == SAVE_PNG){
		gtk_combo_box_set_active(combobox_evo_fileformat, 1);
	} else {
		gtk_combo_box_set_active(combobox_evo_fileformat, 0);
	};
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_evo_fileformat), renderer_evo_fileformat, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_evo_fileformat), renderer_evo_fileformat,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_evo_fileformat), "changed", 
				G_CALLBACK(set_evo_fileformat_CB), NULL);
	
	
	switch_axis = gtk_switch_new();
	if(kemoview_get_object_property_flags(AXIS_TOGGLE) == 0){
		gtk_switch_set_active(GTK_SWITCH(switch_axis), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(switch_axis), TRUE);
	};
	g_signal_connect(G_OBJECT(switch_axis), "notify::active",
				G_CALLBACK(draw_axis_switch_CB), NULL);
	
	switch_coastline = gtk_switch_new();
	if(kemoview_get_object_property_flags(COASTLINE_SWITCH) == 0){
		gtk_switch_set_active(GTK_SWITCH(switch_coastline), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(switch_coastline), TRUE);
	};
	g_signal_connect(G_OBJECT(switch_coastline), "notify::active",
				G_CALLBACK(draw_coastline_switch_CB), NULL);
	
	switch_sph_grid = gtk_switch_new();
	if(kemoview_get_object_property_flags(SPHEREGRID_SWITCH) == 0){
		gtk_switch_set_active(GTK_SWITCH(switch_sph_grid), FALSE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(switch_sph_grid), TRUE);
	};
	g_signal_connect(G_OBJECT(switch_sph_grid), "notify::active",
				G_CALLBACK(draw_sph_grid_switch_CB), NULL);
	
	current_radius = kemoview_get_coastline_radius();
	sprintf(current_radius_text, "    %e    ", current_radius);
	adj_coast_radius = gtk_adjustment_new(current_radius, 0.0, 10.0, 0.02, 0.02, 0.0);
	spin_coast_radius = gtk_spin_button_new(GTK_ADJUSTMENT(adj_coast_radius), 0, 3);
	g_signal_connect(spin_coast_radius, "value-changed", G_CALLBACK(coastline_radius_CB),NULL);
	
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
	
	entry_evo_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_evo_file), "parent", (gpointer) window_main);
	
	evoSelect_Button = gtk_button_new_with_label("Select...");
	g_signal_connect(evoSelect_Button, "clicked", G_CALLBACK(kemoview_gtk_save_file_select),
				(gpointer) entry_evo_file);
	
	
	evoView_Button = gtk_button_new_with_label("View Evolution");
	g_signal_connect(G_OBJECT(evoView_Button), "clicked", 
					 G_CALLBACK(evolution_view_CB), (gpointer)entry_evo_file);
	evoSave_Button = gtk_button_new_with_label("Save Evolution");
	g_signal_connect(G_OBJECT(evoSave_Button), "clicked", 
					 G_CALLBACK(evolution_save_CB), (gpointer)entry_evo_file);
	
	
	int iflag;
    struct kv_string *image_prefix = kemoview_alloc_kvstring();
	int istep = kemoview_get_PSF_full_path_file_prefix(image_prefix, &iflag);
    kemoview_free_kvstring(image_prefix);
	
	current_evo_start = istep;
	sprintf(current_evo_start_text, "    %d    ", current_evo_start);
	adj_evo_start = gtk_adjustment_new(current_evo_start, 0, istep*1000, 1, 1, 0.0);
	spin_evo_start = gtk_spin_button_new(GTK_ADJUSTMENT(adj_evo_start), 0, 3);
	g_signal_connect(spin_evo_start, "value-changed", G_CALLBACK(evo_start_step_CB),NULL);
	
	current_evo_end = istep;
	sprintf(current_evo_end_text, "    %d    ", current_evo_end);
	adj_evo_end = gtk_adjustment_new(current_evo_end, 0.00, istep*1000, 1, 1, 0.0);
	spin_evo_end = gtk_spin_button_new(GTK_ADJUSTMENT(adj_evo_end), 0, 3);
	g_signal_connect(spin_evo_end, "value-changed", G_CALLBACK(evo_end_step_CB),NULL);
	
	current_evo_increment = inc_evo;
	sprintf(current_evo_inc_text, "    %d    ", current_evo_increment);
	adj_evo_increment = gtk_adjustment_new(current_evo_increment, 0, istep*100, 1, 1, 0.0);
	spin_evo_increment = gtk_spin_button_new(GTK_ADJUSTMENT(adj_evo_increment), 0, 3);
	g_signal_connect(spin_evo_increment, "value-changed", G_CALLBACK(evo_increment_CB),NULL);
	
	
	hbox_viewtype = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_viewtype), gtk_label_new("View type: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_viewtype), combobox_viewtype, FALSE, FALSE, 0);
	
	hbox_viewmatrix_save = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_viewmatrix_save), saveView_Button, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_viewmatrix_save), loadView_Button, FALSE, FALSE, 0);
	
	
	hbox_shading = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_shading), gtk_label_new("Shading mode: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_shading), combobox_shading, FALSE, FALSE, 0);
	
	hbox_surf_dir = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_surf_dir), gtk_label_new("Surface direction: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_surf_dir), combobox_surf_dir, FALSE, FALSE, 0);
	
	hbox_axis = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_axis), gtk_label_new("Draw axis: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_axis), switch_axis, FALSE, FALSE, 0);
	
	hbox_coastline = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_coastline), gtk_label_new("Draw coastline: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_coastline), switch_coastline, FALSE, FALSE, 0);
	
	hbox_sph_grid = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_sph_grid), gtk_label_new("Draw sphere grid: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_sph_grid), switch_sph_grid, FALSE, FALSE, 0);
	
	hbox_org_radius = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_org_radius), gtk_label_new("Current distance: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_org_radius), gtk_label_new(current_radius_text), TRUE, TRUE, 0);
	hbox_coast_radius = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_coast_radius), gtk_label_new("Radius: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_coast_radius), spin_coast_radius, TRUE, TRUE, 0);
	
	hbox_rotation_dir = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_dir), gtk_label_new("Surface direction: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_dir), combobox_rotation_dir, FALSE, FALSE, 0);
	
	hbox_org_rot_increment = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_org_rot_increment), gtk_label_new("Current increment: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_org_rot_increment), gtk_label_new(current_rot_inc_text), TRUE, TRUE, 0);
	hbox_rot_increment = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_rot_increment), gtk_label_new("Step (Deg.): "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_rot_increment), spin_rot_increment, TRUE, TRUE, 0);
	
	hbox_rotation_fileformat = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_fileformat), gtk_label_new("File format: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_fileformat), combobox_rotation_fileformat, TRUE, TRUE, 0);
	
	hbox_rotation_filename = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_filename), gtk_label_new("Image file: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_filename), entry_rotation_file, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_filename), rotSelect_Button, FALSE, FALSE, 0);
	
	hbox_rotation_save = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_save), rotView_Button, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_rotation_save), rotSave_Button, FALSE, FALSE, 0);
	
	hbox_evo_start = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_evo_start), gtk_label_new("Start step: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_evo_start), spin_evo_start, TRUE, TRUE, 0);
	hbox_evo_end = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_evo_end), gtk_label_new("End step: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_evo_end), spin_evo_end, TRUE, TRUE, 0);
	hbox_evo_increment = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_evo_increment), gtk_label_new("Increment: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_evo_increment), spin_evo_increment, TRUE, TRUE, 0);
	
	hbox_evo_fileformat = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_evo_fileformat), gtk_label_new("File format: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_evo_fileformat), combobox_evo_fileformat, TRUE, TRUE, 0);
	
	hbox_evo_filename = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_evo_filename), gtk_label_new("Image file: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_evo_filename), entry_evo_file, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_evo_filename), evoSelect_Button, FALSE, FALSE, 0);
	
	hbox_evo_save = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_evo_save), evoView_Button, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_evo_save), evoSave_Button, FALSE, FALSE, 0);
	
	
	vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_container_add(GTK_CONTAINER(window_main), vbox);
	
	gtk_box_pack_start(GTK_BOX(vbox), hbox_viewtype, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_viewmatrix_save, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox), hbox_axis, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_coastline, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_sph_grid, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_org_radius, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_coast_radius, TRUE, TRUE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox), hbox_shading, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_surf_dir, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox), hbox_rotation_dir, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_org_rot_increment, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_rot_increment, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_rotation_filename, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_rotation_fileformat, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_rotation_save, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox), hbox_evo_start, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_evo_end, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_evo_increment, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_evo_filename, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_evo_fileformat, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox_evo_save, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox), updateButton, FALSE, FALSE, 0);
	
	
	gtk_widget_show_all(window_main);
	gtk_main();
	return;
}
