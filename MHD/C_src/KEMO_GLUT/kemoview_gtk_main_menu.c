/*
 *  kemoview_gtk_main_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_main_menu.h"

int id_fmt = 0;

static void kemoview_pref_menu_CB(GtkButton *button, gpointer user_data){
	struct kemoviewer_type *kemoviewer_data = (struct kemoviewer_type *) user_data;
	kemoview_preference_GTK(kemoviewer_data);
	return;
};

static void kemoview_fline_menu_CB(GtkButton *button, gpointer user_data){
	struct kemoviewer_type *kemoviewer_data = (struct kemoviewer_type *) user_data;
	gtk_fieldline_menu();
	return;
};

static void kemoview_mesh_menu_CB(GtkButton *button, gpointer user_data){
	struct kemoviewer_type *kemoviewer_data = (struct kemoviewer_type *) user_data;
	gtk_mesh_menu(kemoviewer_data);
	return;
};

static void set_viewtype_CB(GtkComboBox *combobox_viewtype, gpointer user_data)
{
    int index_mode = gtk_selected_combobox_index(combobox_viewtype);
	
	set_viewtype_mode_glfw(index_mode);
	
	draw_mesh_glfw();
	return;
};

static void save_viewmatrix_CB(GtkButton *button, gpointer user_data){
	int iflag_set = kemoview_gtk_save_file_select(button, user_data);
	if(iflag_set == IZERO) return;
	
	GtkEntry *entry = GTK_ENTRY(user_data);
	struct kv_string *filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
	
	kemoview_write_modelview_file(filename);
	kemoview_free_kvstring(filename);
	
	return;
};
static void load_viewmatrix_CB(GtkButton *button, gpointer user_data){
	
	int iflag_set = kemoview_gtk_read_file_select(button, user_data);
	
	if(iflag_set == IZERO) return;
	GtkEntry *entry = GTK_ENTRY(user_data);
	struct kv_string *filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
	kemoview_load_modelview_file(filename);
	kemoview_free_kvstring(filename);
	
	draw_mesh_glfw();
	return;
};

static void set_image_fileformat_CB(GtkComboBox *combobox_filefmt, gpointer user_data)
{
    int id_fmt = gtk_selected_combobox_index(combobox_filefmt);
	
	draw_mesh_glfw();
	return;
};

static void image_save_CB(GtkButton *button, gpointer user_data){
	int id_image;
	
	int iflag_set = kemoview_gtk_save_file_select(button, user_data);
	if(iflag_set == IZERO) return;
	
	GtkEntry *entry = GTK_ENTRY(user_data);
	struct kv_string *filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
    struct kv_string *stripped_ext = kemoview_alloc_kvstring();
	struct kv_string *file_prefix = kemoview_alloc_kvstring();
	
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

static void close_psf_CB(GtkButton *button, gpointer user_data){
	GtkEntry *entry = GTK_ENTRY(user_data);
	struct kemoviewer_type *kemoviewer_data = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");
	struct colormap_view *color_vws = (struct colormap_view *) g_object_get_data(G_OBJECT(user_data), "colorview");
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	GtkWidget *box = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "box"));
	struct main_buttons *mbot = (struct main_buttons *) g_object_get_data(G_OBJECT(user_data), "buttons");
	
	int nload_psf = kemoview_close_PSF_view();
	
	set_viewtype_mode_glfw(VIEW_3D);
	dealloc_colormap_views_4_viewer(color_vws);
	
	gtk_widget_destroy(mbot->prefButton);
	gtk_widget_destroy(mbot->meshButton);
	gtk_widget_destroy(mbot->flineButton);
	
	gtk_widget_destroy(mbot->evolutionBox);
	gtk_widget_destroy(mbot->rotationBox);
	
	gtk_widget_destroy(mbot->psfBox);
	
	int iflag_draw_m = kemoview_get_draw_mesh_flag();
	int iflag_draw_f = kemoview_get_fline_switch();
	
	mbot->psfBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	if(nload_psf > 0){
		gtk_psf_menu_box(kemoviewer_data, color_vws, mbot, window_main, box);
	};
	gtk_box_pack_start(GTK_BOX(box), mbot->psfBox, FALSE, FALSE, 0);
	
	mbot->rotationBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	add_rotation_menu_box(kemoviewer_data, window_main, mbot->rotationBox);
	gtk_box_pack_start(GTK_BOX(box), mbot->rotationBox, FALSE, FALSE, 0);
	
	mbot->evolutionBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	if(nload_psf > 0 || iflag_draw_f){
		add_evoluaiton_menu_box(kemoviewer_data, window_main, mbot->evolutionBox);
	};
	gtk_box_pack_start(GTK_BOX(box), mbot->evolutionBox, FALSE, FALSE, 0);
	
	
	mbot->prefButton = gtk_button_new_with_label("Preferences...");
	g_signal_connect(G_OBJECT(mbot->prefButton), "clicked", 
				G_CALLBACK(kemoview_pref_menu_CB), (gpointer) kemoviewer_data);
	mbot->flineButton = gtk_button_new_with_label("Field line");
	g_signal_connect(G_OBJECT(mbot->flineButton), "clicked", 
				G_CALLBACK(kemoview_fline_menu_CB), (gpointer) kemoviewer_data);
	mbot->meshButton = gtk_button_new_with_label("Mesh");
	g_signal_connect(G_OBJECT(mbot->meshButton), "clicked", 
				G_CALLBACK(kemoview_mesh_menu_CB), (gpointer) kemoviewer_data);
	
	gtk_box_pack_start(GTK_BOX(box), mbot->flineButton, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box), mbot->meshButton, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box), mbot->prefButton, FALSE, FALSE, 0);
	
	gtk_widget_show_all(box);
	if(nload_psf == 0)gtk_widget_hide(mbot->psfBox);
	if(iflag_draw_f == 0) gtk_widget_hide(mbot->flineButton);	
	if(iflag_draw_m == 0) gtk_widget_hide(mbot->meshButton);
	
	
	gtk_widget_queue_draw(window_main);
	draw_mesh_glfw();
};

static void open_file_CB(GtkButton *button, gpointer user_data){
	int iflag_datatype;
    struct kv_string *filename;
    struct kv_string *file_prefix;
    struct kv_string *stripped_ext;
    struct kv_string *command;
	
	int iflag_set = kemoview_gtk_read_file_select(button, user_data);
	if(iflag_set == IZERO) return;
	GtkEntry *entry = GTK_ENTRY(user_data);
	struct kemoviewer_type *kemoviewer_data = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");
	struct colormap_view *color_vws = (struct colormap_view *) g_object_get_data(G_OBJECT(user_data), "colorview");
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	GtkWidget *box = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "box"));
	struct main_buttons *mbot = (struct main_buttons *) g_object_get_data(G_OBJECT(user_data), "buttons");
	filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
	
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
	
	gtk_widget_destroy(mbot->prefButton);
	gtk_widget_destroy(mbot->meshButton);
	gtk_widget_destroy(mbot->flineButton);
	
	gtk_widget_destroy(mbot->evolutionBox);
	gtk_widget_destroy(mbot->rotationBox);
	
	gtk_widget_destroy(mbot->psfBox);
	
	int iflag_draw_m = kemoview_get_draw_mesh_flag();
	int iflag_draw_f = kemoview_get_fline_switch();
	int nload_psf = kemoview_get_PSF_num_loaded();
	
	mbot->psfBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	if(nload_psf > 0){
		gtk_psf_menu_box(kemoviewer_data, color_vws, mbot, window_main, box);
	};
	gtk_box_pack_start(GTK_BOX(box), mbot->psfBox, FALSE, FALSE, 0);
	
	mbot->rotationBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	add_rotation_menu_box(kemoviewer_data, window_main, mbot->rotationBox);
	gtk_box_pack_start(GTK_BOX(box), mbot->rotationBox, FALSE, FALSE, 0);
	
	mbot->evolutionBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	if(nload_psf > 0 || iflag_draw_f){
		add_evoluaiton_menu_box(kemoviewer_data, window_main, mbot->evolutionBox);
	};
	gtk_box_pack_start(GTK_BOX(box), mbot->evolutionBox, FALSE, FALSE, 0);
	
	
	mbot->prefButton = gtk_button_new_with_label("Preferences...");
	g_signal_connect(G_OBJECT(mbot->prefButton), "clicked", 
				G_CALLBACK(kemoview_pref_menu_CB), (gpointer) kemoviewer_data);
	mbot->flineButton = gtk_button_new_with_label("Field line");
	g_signal_connect(G_OBJECT(mbot->flineButton), "clicked", 
				G_CALLBACK(kemoview_fline_menu_CB), (gpointer) kemoviewer_data);
	mbot->meshButton = gtk_button_new_with_label("Mesh");
	g_signal_connect(G_OBJECT(mbot->meshButton), "clicked", 
				G_CALLBACK(kemoview_mesh_menu_CB), (gpointer) kemoviewer_data);
	
	gtk_box_pack_start(GTK_BOX(box), mbot->flineButton, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box), mbot->meshButton, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box), mbot->prefButton, FALSE, FALSE, 0);
	
	gtk_widget_show_all(box);
	if(nload_psf == 0)gtk_widget_hide(mbot->psfBox);
	if(iflag_draw_f == 0) gtk_widget_hide(mbot->flineButton);	
	if(iflag_draw_m == 0) gtk_widget_hide(mbot->meshButton);
	
	
	
	gtk_main_iteration();
	gtk_widget_queue_draw(window_main);
	draw_mesh_glfw();
	return;
};


void gtk_psf_menu_box(struct kemoviewer_type *kemoviewer_data, struct colormap_view *color_vws,
			struct main_buttons *mbot, GtkWidget *window, GtkWidget *box){
	GtkWidget *closeButton;
	
	GtkWidget *entry_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_file), "kemoview", (gpointer) kemoviewer_data);
	g_object_set_data(G_OBJECT(entry_file), "colorview", (gpointer) color_vws);
	g_object_set_data(G_OBJECT(entry_file), "parent", (gpointer) window);
	g_object_set_data(G_OBJECT(entry_file), "buttons", (gpointer) mbot);
	g_object_set_data(G_OBJECT(entry_file), "box", (gpointer) box);
	
	closeButton = gtk_button_new_with_label("Close Current PSF");
	g_signal_connect(G_OBJECT(closeButton), "clicked", 
				G_CALLBACK(close_psf_CB), entry_file);
	
	
	color_vws->psfBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_box_pack_start(GTK_BOX(color_vws->psfBox), closeButton, FALSE, FALSE, 0);
	make_psf_menu_box(kemoviewer_data, color_vws, window);
	wrap_into_frame_gtk("Surfaces", color_vws->psfBox, mbot->psfBox);
	gtk_widget_show(mbot->psfBox);
	return;
}

void make_gtk_main_menu_box(struct kemoviewer_type *kemoviewer_data,
			GtkWidget *window_main, GtkWidget *box){
	struct main_buttons *mbot = (struct main_buttons *) malloc(sizeof(struct main_buttons));
	struct colormap_view *color_vws = (struct colormap_view *) malloc(sizeof(struct colormap_view));
	
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
	
	/* Set buttons   */
	mbot->prefButton = gtk_button_new_with_label("Preferences...");
	g_signal_connect(G_OBJECT(mbot->prefButton), "clicked", 
				G_CALLBACK(kemoview_pref_menu_CB), (gpointer) kemoviewer_data);
	
	
	mbot->flineButton = gtk_button_new_with_label("Field line");
	g_signal_connect(G_OBJECT(mbot->flineButton), "clicked", 
				G_CALLBACK(kemoview_fline_menu_CB), (gpointer) kemoviewer_data);
	
	mbot->meshButton = gtk_button_new_with_label("Mesh");
	g_signal_connect(G_OBJECT(mbot->meshButton), "clicked", 
				G_CALLBACK(kemoview_mesh_menu_CB), (gpointer) kemoviewer_data);
	
	
	box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	entry_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_file), "kemoview", (gpointer) kemoviewer_data);
	g_object_set_data(G_OBJECT(entry_file), "colorview", (gpointer) color_vws);
	g_object_set_data(G_OBJECT(entry_file), "parent", (gpointer) window_main);
	g_object_set_data(G_OBJECT(entry_file), "buttons", (gpointer) mbot);
	g_object_set_data(G_OBJECT(entry_file), "box", (gpointer) box);
	open_Button = gtk_button_new_with_label("Open...");
	g_signal_connect(G_OBJECT(open_Button), "clicked", 
					 G_CALLBACK(open_file_CB), (gpointer)entry_file);
	
	entry_image_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_image_file), "parent", (gpointer) window_main);
	imageSave_Button = gtk_button_new_with_label("Save Image...");
	g_signal_connect(G_OBJECT(imageSave_Button), "clicked", 
					 G_CALLBACK(image_save_CB), (gpointer)entry_image_file);
	
	
	label_tree_viewtype = create_fixed_label_w_index_tree();
	model_viewtype = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_viewtype));  
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
	model_image_fileformat = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_image_fileformat));  
	child_model_image_fileformat = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_image_fileformat));
	index = 0;
	index = append_ci_item_to_tree(index, "No Image", NO_SAVE_FILE, child_model_image_fileformat);
	index = append_ci_item_to_tree(index, "PNG", SAVE_PNG, child_model_image_fileformat);
	index = append_ci_item_to_tree(index, "BMP", SAVE_BMP, child_model_image_fileformat);
	
	combobox_image_fileformat = gtk_combo_box_new_with_model(child_model_image_fileformat);
	renderer_image_fileformat = gtk_cell_renderer_text_new();
	id_fmt = NO_SAVE_FILE;
	if(id_fmt == SAVE_BMP){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_image_fileformat), 2);
	} else if(id_fmt == SAVE_PNG){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_image_fileformat), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_image_fileformat), 0);
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
	
	gtk_container_add(GTK_CONTAINER(window_main), box);
	
	gtk_box_pack_start(GTK_BOX(box), hbox_open, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box), hbox_image_save, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(box), hbox_viewtype, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box), hbox_viewmatrix_save, FALSE, FALSE, 0);
	
	add_axis_menu_box(kemoviewer_data, box);
	
	mbot->psfBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	if(nload_psf > 0){
		gtk_psf_menu_box(kemoviewer_data, color_vws, mbot, window_main, box);
	};
	gtk_box_pack_start(GTK_BOX(box), mbot->psfBox, FALSE, FALSE, 0);
	
	mbot->rotationBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	add_rotation_menu_box(kemoviewer_data, window_main, mbot->rotationBox);
	gtk_box_pack_start(GTK_BOX(box), mbot->rotationBox, FALSE, FALSE, 0);

	mbot->evolutionBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	if(nload_psf > 0 || iflag_draw_f){
		add_evoluaiton_menu_box(kemoviewer_data, window_main, mbot->evolutionBox);
	};
	gtk_box_pack_start(GTK_BOX(box), mbot->evolutionBox, FALSE, FALSE, 0);

	gtk_box_pack_start(GTK_BOX(box), mbot->flineButton, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box), mbot->meshButton, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box), mbot->prefButton, FALSE, FALSE, 0);
	
	gtk_widget_show_all(box);
	if(nload_psf == 0) gtk_widget_hide(mbot->psfBox);
	if(iflag_draw_f == 0) gtk_widget_hide(mbot->flineButton);
	if(iflag_draw_m == 0) gtk_widget_hide(mbot->meshButton);
	
	if(nload_psf > 0){
		dealloc_colormap_views_4_viewer(color_vws);
		free(color_vws);
	};
}
