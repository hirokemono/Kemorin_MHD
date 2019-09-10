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

static void kemoview_mesh_menu_CB(GtkButton *button, gpointer user_data){
	struct kemoviewer_type *kemoviewer_data = (struct kemoviewer_type *) user_data;
	gtk_mesh_menu(kemoviewer_data);
	return;
};

static void delete_kemoview_menu(struct main_buttons *mbot){
	gtk_widget_destroy(mbot->prefBox);
	gtk_widget_destroy(mbot->meshButton);
	gtk_widget_destroy(mbot->flineButton);
	
	gtk_widget_destroy(mbot->viewBox);
	gtk_widget_destroy(mbot->evolutionBox);
	gtk_widget_destroy(mbot->rotationBox);
	
	gtk_widget_destroy(mbot->psfBox);
	return;
};

static void update_kemoview_menu(struct kemoviewer_type *kemoviewer_data, 
			struct main_buttons *mbot, GtkWidget *window){
	int iflag_draw_m = kemoview_get_draw_mesh_flag();
	int iflag_draw_f = kemoview_get_fline_switch();
	int nload_psf = kemoview_get_PSF_num_loaded();
	
	mbot->meshButton = gtk_button_new_with_label("Mesh");
	g_signal_connect(G_OBJECT(mbot->meshButton), "clicked", 
				G_CALLBACK(kemoview_mesh_menu_CB), (gpointer) kemoviewer_data);
	
	
	mbot->psfBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	if(nload_psf > 0){
		gtk_psf_menu_box(kemoviewer_data, mbot, window);
	};
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), mbot->psfBox, FALSE, FALSE, 0);
	
	mbot->flineButton = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	if(iflag_draw_f > 0){
		gtk_fieldline_menu_box(kemoviewer_data, mbot, window);
	};
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), mbot->flineButton, FALSE, FALSE, 0);
	
	mbot->rotationBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	add_rotation_menu_box(kemoviewer_data, window, mbot->rotationBox);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), mbot->rotationBox, FALSE, FALSE, 0);
	
	mbot->evolutionBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	if(nload_psf > 0 || iflag_draw_f > 0){
		int istep, iflag;
		struct kv_string *file_prefix = kemoview_alloc_kvstring();
		if(nload_psf > 0){
			istep = kemoview_get_PSF_full_path_file_prefix(file_prefix, &iflag);
		} else {
			istep = kemoview_get_fline_file_step_prefix(file_prefix);
		};
		kemoview_free_kvstring(file_prefix);
		
		add_evoluaiton_menu_box(istep, kemoviewer_data, window, mbot->evolutionBox);
	};
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), mbot->evolutionBox, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), mbot->meshButton, FALSE, FALSE, 0);
	
	mbot->viewBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_viewmatrix_menu_box(mbot->view_menu, window, mbot->viewBox);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), mbot->viewBox, FALSE, FALSE, 0);
	
	mbot->prefBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	kemoview_preference_GTK(kemoviewer_data, mbot->lightparams_vws, mbot->prefBox);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), mbot->prefBox, FALSE, FALSE, 0);
	
	gtk_widget_show_all(mbot->vbox_menu);
	if(nload_psf == 0) gtk_widget_hide(mbot->psfBox);
	if(iflag_draw_f == 0) gtk_widget_hide(mbot->flineButton);
	if(iflag_draw_m == 0) gtk_widget_hide(mbot->meshButton);
	return;
};


static void set_viewtype_CB(GtkComboBox *combobox_viewtype, gpointer user_data)
{
	GtkEntry *entry = GTK_ENTRY(user_data);
	struct kemoviewer_type *kemoviewer_data = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct main_buttons *mbot = (struct main_buttons *) g_object_get_data(G_OBJECT(user_data), "buttons");
	
	int index_mode = gtk_selected_combobox_index(combobox_viewtype);
	
	set_viewtype_mode_glfw(index_mode);
	
	delete_kemoview_menu(mbot);
	update_kemoview_menu(kemoviewer_data, mbot, window_main);
	
	gtk_widget_queue_draw(window_main);
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

static void current_psf_select_CB(GtkComboBox *combobox_sfcolor, gpointer user_data)
{
	GtkEntry *entry = GTK_ENTRY(user_data);
	struct kemoviewer_type *kemoviewer_data = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct main_buttons *mbot = (struct main_buttons *) g_object_get_data(G_OBJECT(user_data), "buttons");

    int index_mode = gtk_selected_combobox_index(combobox_sfcolor);
	
	kemoview_set_current_PSF(index_mode);
	
	dealloc_colormap_views_4_viewer(mbot->color_vws);
	
	delete_kemoview_menu(mbot);
	update_kemoview_menu(kemoviewer_data, mbot, window_main);
	
	
	gtk_widget_queue_draw(window_main);
	draw_mesh_glfw();
	return;
};

static void close_psf_CB(GtkButton *button, gpointer user_data){
	GtkEntry *entry = GTK_ENTRY(user_data);
	struct kemoviewer_type *kemoviewer_data = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct main_buttons *mbot = (struct main_buttons *) g_object_get_data(G_OBJECT(user_data), "buttons");
	
	int nload_psf = kemoview_close_PSF_view();
	
	set_viewtype_mode_glfw(VIEW_3D);
	dealloc_colormap_views_4_viewer(mbot->color_vws);
	
	delete_kemoview_menu(mbot);
	update_kemoview_menu(kemoviewer_data, mbot, window_main);
	
	gtk_widget_queue_draw(window_main);
	draw_mesh_glfw();
};

static void close_fline_CB(GtkButton *button, gpointer user_data){
	GtkEntry *entry = GTK_ENTRY(user_data);
	struct kemoviewer_type *kemoviewer_data = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct main_buttons *mbot = (struct main_buttons *) g_object_get_data(G_OBJECT(user_data), "buttons");
	
	delete_kemoview_menu(mbot);
	update_kemoview_menu(kemoviewer_data, mbot, window_main);
	
	gtk_widget_queue_draw(window_main);
	draw_mesh_glfw();
};

static void psf_field_select_CB(GtkComboBox *combobox_field, gpointer user_data)
{
	GtkEntry *entry = GTK_ENTRY(user_data);
	struct kemoviewer_type *kemoviewer_data = (struct kemoviewer_type *) g_object_get_data(G_OBJECT(user_data), "kemoview");
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct main_buttons *mbot = (struct main_buttons *) g_object_get_data(G_OBJECT(user_data), "buttons");
	
    int index_mode = gtk_selected_combobox_index(combobox_field);
    
	kemoview_set_PSF_field(index_mode);
	
	dealloc_colormap_views_4_viewer(mbot->color_vws);
	
	delete_kemoview_menu(mbot);
	update_kemoview_menu(kemoviewer_data, mbot, window_main);
	
	
	gtk_widget_queue_draw(window_main);
	draw_mesh_glfw();
	return;
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
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
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
	
	delete_kemoview_menu(mbot);
	update_kemoview_menu(kemoviewer_data, mbot, window_main);
	
	gtk_main_iteration();
	gtk_widget_queue_draw(window_main);
	draw_mesh_glfw();
	return;
};


void add_current_psf_set_box(struct kemoviewer_type *kemoviewer_data,
			struct main_buttons *mbot, GtkWidget *window, GtkWidget *box_out){
	GtkWidget *hbox_psfs;
	
	GtkWidget *combobox_psfs;
	GtkWidget *label_tree_psfs;
	GtkCellRenderer *renderer_psfs;
	GtkTreeModel *model_psfs;
	GtkTreeModel *child_model_psfs;
	
	int index = 0;
	int ipsf, istep;
	
	struct kv_string *stripped_filehead;
	char label_tmp[512];
	int id_current = kemoview_get_curent_PSF_ID();
	int index_current;
	int num_psf = 0;
	for (ipsf=0; ipsf< kemoview_get_PSF_max_loaded(); ipsf++){
		if(kemoview_get_PSF_loaded_flag(ipsf) > 0) {num_psf = num_psf + 1;};
	};
	
	GtkWidget *entry;
	entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer) window);
	g_object_set_data(G_OBJECT(entry), "colorview", (gpointer) mbot->color_vws);
	
	if(num_psf > 1){
		label_tree_psfs = create_fixed_label_w_index_tree();
		model_psfs = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_psfs));  
		child_model_psfs = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_psfs));
		index = 0;
		for (ipsf=0; ipsf< kemoview_get_PSF_max_loaded(); ipsf++){
			if(ipsf == id_current) {index_current = index;};
			if(kemoview_get_PSF_loaded_flag(ipsf) > 0) {
				kemoview_set_current_PSF(ipsf);
				stripped_filehead = kemoview_alloc_kvstring();
				istep = kemoview_get_PSF_file_prefix(stripped_filehead);
				sprintf(label_tmp, "%d: %s", ipsf, stripped_filehead->string);
				index = append_ci_item_to_tree(index, label_tmp,
											   ipsf, child_model_psfs);
				kemoview_free_kvstring(stripped_filehead);
			};
			kemoview_set_current_PSF(id_current);
		};
		
		GtkWidget *entry_file = gtk_entry_new();
		g_object_set_data(G_OBJECT(entry_file), "kemoview", (gpointer) kemoviewer_data);
		g_object_set_data(G_OBJECT(entry_file), "parent", (gpointer) window);
		g_object_set_data(G_OBJECT(entry_file), "buttons", (gpointer) mbot);
		
		combobox_psfs = gtk_combo_box_new_with_model(child_model_psfs);
		renderer_psfs = gtk_cell_renderer_text_new();
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_psfs), index_current);
		gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_psfs), renderer_psfs, TRUE);
		gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_psfs), renderer_psfs,
					"text", COLUMN_FIELD_NAME, NULL);
		g_signal_connect(G_OBJECT(combobox_psfs), "changed", 
					G_CALLBACK(current_psf_select_CB), (gpointer) entry_file);
		
		hbox_psfs = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
		gtk_box_pack_start(GTK_BOX(hbox_psfs), gtk_label_new("Current PSF: "), FALSE, FALSE, 0);
		gtk_box_pack_start(GTK_BOX(hbox_psfs), combobox_psfs, FALSE, FALSE, 0);
		
		gtk_box_pack_start(GTK_BOX(box_out), hbox_psfs, TRUE, TRUE, 0);
	}
	
	return;
}


void add_psf_draw_field_box(struct kemoviewer_type *kemoviewer_data, 
			struct main_buttons *mbot, GtkWidget *window, GtkWidget *box_out){
	GtkWidget *hbox_field;
	
	GtkWidget *combobox_field;
	GtkWidget *label_tree_field;
	GtkCellRenderer *renderer_field;
	GtkTreeModel *model_field;
	GtkTreeModel *child_model_field;
	
	int index = 0;
	
    struct kv_string *colorname = kemoview_alloc_kvstring();
	int num_field = kemoview_get_PSF_num_field();
	int if_psf = kemoview_get_PSF_field_id();
	int ic_psf = kemoview_get_PSF_component_id();
	int ifld;
	
	GtkWidget *entry;
	entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer) window);
	g_object_set_data(G_OBJECT(entry), "colorview", (gpointer) mbot->color_vws);
	g_object_set_data(G_OBJECT(entry), "kemoview", (gpointer) kemoviewer_data);
	
	label_tree_field = create_fixed_label_w_index_tree();
	model_field = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_field));  
	child_model_field = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_field));
	
	index = 0;
	for(ifld=0;ifld<num_field;ifld++){
		kemoview_get_PSF_field_name(colorname, ifld);
		index = append_ci_item_to_tree(index, colorname->string, ifld, child_model_field);
	};
	
	GtkWidget *entry_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_file), "kemoview", (gpointer) kemoviewer_data);
	g_object_set_data(G_OBJECT(entry_file), "colorview", (gpointer) mbot->color_vws);
	g_object_set_data(G_OBJECT(entry_file), "parent", (gpointer) window);
	g_object_set_data(G_OBJECT(entry_file), "buttons", (gpointer) mbot);
		
	combobox_field = gtk_combo_box_new_with_model(child_model_field);
	renderer_field = gtk_cell_renderer_text_new();
	gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_field), if_psf);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_field), renderer_field, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_field), renderer_field,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(combobox_field), "changed", 
				G_CALLBACK(psf_field_select_CB), (gpointer) entry_file);
	
	hbox_field = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_field), gtk_label_new("Field: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_field), combobox_field, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(box_out), hbox_field, TRUE, TRUE, 0);
	return;
}


void gtk_psf_menu_box(struct kemoviewer_type *kemoviewer_data, 
			struct main_buttons *mbot, GtkWidget *window){
	GtkWidget *closeButton;
	GtkWidget *vbox;
	
	GtkWidget *entry_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_file), "kemoview", (gpointer) kemoviewer_data);
	g_object_set_data(G_OBJECT(entry_file), "parent", (gpointer) window);
	g_object_set_data(G_OBJECT(entry_file), "buttons", (gpointer) mbot);
	
	closeButton = gtk_button_new_with_label("Close Current PSF");
	g_signal_connect(G_OBJECT(closeButton), "clicked", 
				G_CALLBACK(close_psf_CB), entry_file);
	
	
	vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_box_pack_start(GTK_BOX(vbox), closeButton, FALSE, FALSE, 0);
	add_current_psf_set_box(kemoviewer_data, mbot, window, vbox);
	add_psf_draw_field_box(kemoviewer_data, mbot, window, vbox);
	make_psf_menu_box(kemoviewer_data, mbot->color_vws, window, vbox);
	wrap_into_frame_gtk("Surfaces", vbox, mbot->psfBox);
	
	gtk_widget_show(mbot->psfBox);
	return;
}


void gtk_fieldline_menu_box(struct kemoviewer_type *kemoviewer_data,
			struct main_buttons *mbot, GtkWidget *window){
	GtkButton *closeButton;
	GtkWidget *vbox;
	
	GtkWidget *entry_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_file), "kemoview", (gpointer) kemoviewer_data);
	g_object_set_data(G_OBJECT(entry_file), "parent", (gpointer) window);
	g_object_set_data(G_OBJECT(entry_file), "buttons", (gpointer) mbot);
		
	closeButton = gtk_button_new_with_label("Close Current PSF");
	g_signal_connect(G_OBJECT(closeButton), "clicked", 
				G_CALLBACK(close_fline_CB), entry_file);
	
	vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_box_pack_start(GTK_BOX(vbox), closeButton, FALSE, FALSE, 0);
	add_gtk_fieldline_menu(vbox);
	wrap_into_frame_gtk("Fieldline", vbox, mbot->flineButton);
	
	gtk_widget_show(mbot->flineButton);
	
	return;
}


void make_gtk_main_menu_box(struct kemoviewer_type *kemoviewer_data,
			struct main_buttons *mbot, GtkWidget *window_main){	
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
	
	
	int index = 0;
	int iflag_mode;
	
	int iflag_draw_m = kemoview_get_draw_mesh_flag();
	int iflag_draw_p = kemoview_get_PSF_draw_switch();
	int iflag_draw_f = kemoview_get_fline_switch();
	int iflag_any_objects_on = iflag_draw_p + iflag_draw_m + iflag_draw_f;
	
	/* Set buttons   */
	entry_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_file), "kemoview", (gpointer) kemoviewer_data);
	g_object_set_data(G_OBJECT(entry_file), "parent", (gpointer) window_main);
	g_object_set_data(G_OBJECT(entry_file), "buttons", (gpointer) mbot);
	
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
				G_CALLBACK(set_viewtype_CB), entry_file);
	
	
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
	
//	gtk_container_add(GTK_CONTAINER(window_main), mbot->vbox_menu);
	
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), hbox_open, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), hbox_image_save, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), hbox_viewtype, FALSE, FALSE, 0);

	
	add_axis_menu_box(kemoviewer_data, mbot->vbox_menu);
	
	update_kemoview_menu(kemoviewer_data, mbot, window_main);
}
