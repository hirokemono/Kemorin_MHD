/*
 *  kemoview_gtk_main_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_main_menu.h"

struct main_buttons * init_main_buttons(struct kemoviewer_type *kemoviewer_data){
	struct main_buttons *mbot = (struct main_buttons *) malloc(sizeof(struct main_buttons));
	
	mbot->view_menu = (struct view_widgets *) malloc(sizeof(struct view_widgets));
	mbot->mesh_vws = (struct kemoview_mesh_view *) malloc(sizeof(struct kemoview_mesh_view));
	mbot->fline_menu = (struct fieldline_gtk_menu *) malloc(sizeof(struct fieldline_gtk_menu));

	mbot->psf_gmenu = alloc_psf_gtk_menu();
	mbot->evo_gmenu = init_evoluaiton_menu_box();
	mbot->rot_gmenu = init_rotation_menu_box();
	mbot->pref_gmenu = init_preference_gtk_menu(kemoviewer_data);
	return mbot;
};

void dealloc_main_buttons(struct main_buttons *mbot){
	dealloc_psf_gtk_menu(mbot->psf_gmenu);
	dealloc_preference_gtk_menu(mbot->pref_gmenu);
	
	free(mbot->fline_menu);
	free(mbot->mesh_vws);
	free(mbot->rot_gmenu);
	free(mbot->evo_gmenu);
	free(mbot->view_menu);
	
	free(mbot);
	return;
};


static void delete_kemoview_menu(struct main_buttons *mbot){
	gtk_widget_destroy(mbot->meshBox);
	gtk_widget_destroy(mbot->evolutionBox);
	gtk_widget_destroy(mbot->flineBox);
	gtk_widget_destroy(mbot->psfBox);
	return;
};

static void update_kemoview_menu(struct main_buttons *mbot, GtkWidget *window){
	int istep, iflag;
	int iflag_draw_m = kemoview_get_draw_mesh_flag();
	int iflag_draw_f = kemoview_get_fline_parameters(DRAW_SWITCH);
	int nload_psf = kemoview_get_PSF_loaded_params(NUM_LOADED);
	
	mbot->psfBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	if(nload_psf > 0){gtk_psf_menu_box(mbot, window);};
	
	mbot->flineBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	if(iflag_draw_f > 0){gtk_fieldline_menu_box(mbot, window);};
	
	mbot->evolutionBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	if(nload_psf > 0 || iflag_draw_f > 0){
		struct kv_string *file_prefix = kemoview_alloc_kvstring();
		if(nload_psf > 0){
			istep = kemoview_get_PSF_full_path_file_prefix(file_prefix, &iflag);
			GtkWidget *expand_psf_evo = init_evoluaiton_menu_expander(istep, window, mbot->evo_gmenu);
            gtk_box_pack_start(GTK_BOX(mbot->evolutionBox), expand_psf_evo, FALSE, FALSE, 0);
			gtk_box_pack_start(GTK_BOX(mbot->psfBox), mbot->evolutionBox, FALSE, FALSE, 0);
		} else {
			istep = kemoview_get_fline_file_step_prefix(file_prefix);
			GtkWidget *expand_fline_evo = init_evoluaiton_menu_expander(istep, window, mbot->evo_gmenu);
            gtk_box_pack_start(GTK_BOX(mbot->evolutionBox), expand_fline_evo, FALSE, FALSE, 0);
			gtk_box_pack_start(GTK_BOX(mbot->flineBox), mbot->evolutionBox, FALSE, FALSE, 0);
		};
		kemoview_free_kvstring(file_prefix);
	};
	
	mbot->meshBox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	if(iflag_draw_m > 0){
		gtk_mesh_menu_box(mbot, window);
	};
	
	gtk_box_pack_start(GTK_BOX(mbot->menuHbox), mbot->psfBox, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->menuHbox), mbot->flineBox, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->menuHbox), mbot->meshBox, FALSE, FALSE, 0);
	
	gtk_window_resize(GTK_WINDOW(window), 240, 200);
	
	
	gtk_widget_show_all(mbot->menuHbox);
	if(nload_psf == 0) gtk_widget_hide(mbot->psfBox);
	if(iflag_draw_f == 0) gtk_widget_hide(mbot->flineBox);
	if(iflag_draw_m == 0) gtk_widget_hide(mbot->meshBox);
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
	
	delete_kemoview_menu(mbot);
	update_kemoview_menu(mbot, window_main);
	gtk_widget_queue_draw(window_main);
	draw_full();
	return;
};

static void set_viewtype_CB(GtkComboBox *combobox_viewtype, gpointer user_data)
{
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct main_buttons *mbot = (struct main_buttons *) g_object_get_data(G_OBJECT(user_data), "buttons");
	
	int index_mode = gtk_selected_combobox_index(combobox_viewtype);
	
	set_viewtype_mode(index_mode);
	
	delete_kemoview_menu(mbot);
	update_kemoview_menu(mbot, window_main);
	
	gtk_widget_queue_draw(window_main);
	draw_full();
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
	if(iflag_set == IZERO) return;
	
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
    kemoview_write_window_to_file(id_imagefmt_by_input, file_prefix);
    kemoview_free_kvstring(file_prefix);
	
	return;
};

static void current_psf_select_CB(GtkComboBox *combobox_sfcolor, gpointer user_data)
{
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct main_buttons *mbot = (struct main_buttons *) g_object_get_data(G_OBJECT(user_data), "buttons");

    int index_mode = gtk_selected_combobox_index(combobox_sfcolor);
	
	kemoview_set_PSF_loaded_params(SET_CURRENT, index_mode);
	dealloc_colormap_views_4_viewer(mbot->psf_gmenu->color_vws);
	
	delete_kemoview_menu(mbot);
	update_kemoview_menu(mbot, window_main);
	
	
	gtk_widget_queue_draw(window_main);
	draw_full();
	return;
};

static void close_psf_CB(GtkButton *button, gpointer user_data){
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct main_buttons *mbot = (struct main_buttons *) g_object_get_data(G_OBJECT(user_data), "buttons");

    int num_loaded = kemoview_close_PSF_view();

    set_viewtype_mode(VIEW_3D);
	dealloc_colormap_views_4_viewer(mbot->psf_gmenu->color_vws);
	
	delete_kemoview_menu(mbot);
	update_kemoview_menu(mbot, window_main);
	
	gtk_widget_queue_draw(window_main);
	draw_full();
};

static void close_fline_CB(GtkButton *button, gpointer user_data){
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct main_buttons *mbot = (struct main_buttons *) g_object_get_data(G_OBJECT(user_data), "buttons");
	
	kemoview_close_fieldline_view();
	
	delete_kemoview_menu(mbot);
	update_kemoview_menu(mbot, window_main);
	
	gtk_widget_queue_draw(window_main);
	draw_full();
};

static void close_mesh_CB(GtkButton *button, gpointer user_data){
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct main_buttons *mbot = (struct main_buttons *) g_object_get_data(G_OBJECT(user_data), "buttons");
	
	kemoview_close_mesh_view();
	dealloc_mesh_views_4_viewer(mbot->mesh_vws);
	
	delete_kemoview_menu(mbot);
	update_kemoview_menu(mbot, window_main);
	
	gtk_widget_queue_draw(window_main);
	draw_full();
	return;
};

static void psf_field_select_CB(GtkComboBox *combobox_field, gpointer user_data)
{
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct main_buttons *mbot = (struct main_buttons *) g_object_get_data(G_OBJECT(user_data), "buttons");
	
    int index_mode = gtk_selected_combobox_index(combobox_field);
    
	kemoview_set_each_PSF_field_param(FIELD_SEL_FLAG, index_mode);
	
	dealloc_colormap_views_4_viewer(mbot->psf_gmenu->color_vws);
	
	delete_kemoview_menu(mbot);
	update_kemoview_menu(mbot, window_main);
	
	
	gtk_widget_queue_draw(window_main);
	draw_full();
	return;
};

static void psf_component_select_CB(GtkComboBox *combobox_comp, gpointer user_data)
{
	GtkWidget *window_main = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct main_buttons *mbot = (struct main_buttons *) g_object_get_data(G_OBJECT(user_data), "buttons");
    
    int index_mode = gtk_selected_combobox_index(combobox_comp);
	
	kemoview_set_each_PSF_field_param(COMPONENT_SEL_FLAG, index_mode);
	
	dealloc_colormap_views_4_viewer(mbot->psf_gmenu->color_vws);
	
	delete_kemoview_menu(mbot);
	update_kemoview_menu(mbot, window_main);
	
	
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


static GtkWidget * init_current_psf_set_vbox(struct main_buttons *mbot, GtkWidget *window){
	GtkWidget *box_out;
	
	int index = 0;
	int ipsf, istep;
	
	struct kv_string *stripped_filehead;
	char label_tmp[512];
	int id_current = kemoview_get_PSF_loaded_params(SET_CURRENT);
	int index_current = 0;
	int num_psf = 0;
	for (ipsf=0; ipsf< kemoview_get_PSF_loaded_params(MAX_LOADED); ipsf++){
		if(kemoview_get_PSF_loaded_flag(ipsf) > 0) {num_psf = num_psf + 1;};
	};
	
	GtkWidget *entry;
	entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer) window);
	g_object_set_data(G_OBJECT(entry), "colorview", (gpointer) mbot->psf_gmenu->color_vws);
	    
    box_out = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
    gtk_box_pack_start(GTK_BOX(box_out), mbot->psf_gmenu->closeButton, FALSE, FALSE, 0);
	if(num_psf > 1){
		GtkWidget *label_tree_psfs = create_fixed_label_w_index_tree();
		GtkTreeModel *model_psfs = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_psfs));  
		GtkTreeModel *child_model_psfs = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_psfs));
		index = 0;
		for (ipsf=0; ipsf< kemoview_get_PSF_loaded_params(MAX_LOADED); ipsf++){
			if(ipsf == id_current) {index_current = index;};
			if(kemoview_get_PSF_loaded_flag(ipsf) > 0) {
				kemoview_set_PSF_loaded_params(SET_CURRENT, ipsf);
				stripped_filehead = kemoview_alloc_kvstring();
				istep = kemoview_get_PSF_file_prefix(stripped_filehead);
				sprintf(label_tmp, "%d: %s", ipsf, stripped_filehead->string);
				index = append_ci_item_to_tree(index, label_tmp,
											   ipsf, child_model_psfs);
				kemoview_free_kvstring(stripped_filehead);
			};
			kemoview_set_PSF_loaded_params(SET_CURRENT, id_current);
		};
		
		GtkWidget *entry_file = gtk_entry_new();
		g_object_set_data(G_OBJECT(entry_file), "parent", (gpointer) window);
		g_object_set_data(G_OBJECT(entry_file), "buttons", (gpointer) mbot);
		
		GtkWidget *combobox_psfs = gtk_combo_box_new_with_model(child_model_psfs);
		GtkCellRenderer *renderer_psfs = gtk_cell_renderer_text_new();
		gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_psfs), index_current);
		gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_psfs), renderer_psfs, TRUE);
		gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_psfs), renderer_psfs,
					"text", COLUMN_FIELD_NAME, NULL);
		g_signal_connect(G_OBJECT(combobox_psfs), "changed", 
					G_CALLBACK(current_psf_select_CB), (gpointer) entry_file);
		
		GtkWidget *hbox_psfs = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
		gtk_box_pack_start(GTK_BOX(hbox_psfs), gtk_label_new("Current PSF: "), FALSE, FALSE, 0);
		gtk_box_pack_start(GTK_BOX(hbox_psfs), combobox_psfs, FALSE, FALSE, 0);
		
		gtk_box_pack_start(GTK_BOX(box_out), hbox_psfs, TRUE, TRUE, 0);
	}
	
	return box_out;
}

static GtkWidget * init_psf_draw_field_hbox(struct main_buttons *mbot, GtkWidget *window){
	GtkWidget *hbox_field;
	
	struct kv_string *colorname = kemoview_alloc_kvstring();
	int num_field = kemoview_get_each_PSF_field_param(NUM_FIELD_FLAG);
	int if_psf =    kemoview_get_each_PSF_field_param(FIELD_SEL_FLAG);
	int ifld;
	
	GtkWidget *entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer) window);
	g_object_set_data(G_OBJECT(entry), "colorview", (gpointer) mbot->psf_gmenu->color_vws);
	
	GtkWidget *label_tree_field = create_fixed_label_w_index_tree();
	GtkTreeModel *model_field = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_field));  
	GtkTreeModel *child_model_field = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_field));
	
	int index = 0;
	for(ifld=0;ifld<num_field;ifld++){
		kemoview_get_PSF_field_name(colorname, ifld);
		index = append_ci_item_to_tree(index, colorname->string, ifld, child_model_field);
	};
	
	GtkWidget *entry_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_file), "parent", (gpointer) window);
	g_object_set_data(G_OBJECT(entry_file), "buttons", (gpointer) mbot);
	
	GtkCellRenderer *renderer_field = gtk_cell_renderer_text_new();
	mbot->psf_gmenu->combobox_field = gtk_combo_box_new_with_model(child_model_field);
	gtk_combo_box_set_active(GTK_COMBO_BOX(mbot->psf_gmenu->combobox_field), if_psf);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(mbot->psf_gmenu->combobox_field), renderer_field, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(mbot->psf_gmenu->combobox_field), renderer_field,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(mbot->psf_gmenu->combobox_field), "changed", 
				G_CALLBACK(psf_field_select_CB), (gpointer) entry_file);
	
	hbox_field = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_field), gtk_label_new("Field: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_field), mbot->psf_gmenu->combobox_field, FALSE, FALSE, 0);
	return hbox_field;
}

static GtkWidget * init_psf_draw_component_hbox(struct main_buttons *mbot, GtkWidget *window){
    GtkWidget *hbox_comp = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	
	char comp_name[1024];
	int if_psf = kemoview_get_each_PSF_field_param(FIELD_SEL_FLAG);
	int ncomp = kemoview_get_PSF_num_component(if_psf);
	int icomp;
	
	if(ncomp < 2) return hbox_comp;
	
	GtkWidget *entry_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_file), "parent", (gpointer) window);
	g_object_set_data(G_OBJECT(entry_file), "buttons", (gpointer) mbot);
	
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
	mbot->psf_gmenu->combobox_comp = gtk_combo_box_new_with_model(child_model_comp);
	gtk_combo_box_set_active(GTK_COMBO_BOX(mbot->psf_gmenu->combobox_comp), icomp);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(mbot->psf_gmenu->combobox_comp), renderer_comp, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(mbot->psf_gmenu->combobox_comp), renderer_comp,
				"text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(mbot->psf_gmenu->combobox_comp), "changed", 
				G_CALLBACK(psf_component_select_CB), (gpointer) entry_file);
	
	gtk_box_pack_start(GTK_BOX(hbox_comp), gtk_label_new("Component: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_comp), mbot->psf_gmenu->combobox_comp, FALSE, FALSE, 0);
	return hbox_comp;
}


void gtk_psf_menu_box(struct main_buttons *mbot, GtkWidget *window){
	
	GtkWidget *entry_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_file), "parent", (gpointer) window);
	g_object_set_data(G_OBJECT(entry_file), "buttons", (gpointer) mbot);
	
	mbot->psf_gmenu->closeButton = gtk_button_new_with_label("Close Current PSF");
	g_signal_connect(G_OBJECT(mbot->psf_gmenu->closeButton), "clicked", 
				G_CALLBACK(close_psf_CB), entry_file);
	
	GtkWidget *hbox_field = init_psf_draw_field_hbox(mbot, window);
	GtkWidget *hbox_comp = init_psf_draw_component_hbox(mbot, window);

    GtkWidget *psf_vbox = init_current_psf_set_vbox(mbot, window);
	gtk_box_pack_start(GTK_BOX(psf_vbox), hbox_field, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(psf_vbox), hbox_comp, TRUE, TRUE, 0);
	
	init_colormap_views_4_viewer(mbot->psf_gmenu->color_vws);
	
	GtkWidget *hbox = init_psf_menu_hbox(mbot->psf_gmenu, window, psf_vbox);
    gtk_box_pack_start(GTK_BOX(mbot->psfBox), hbox, FALSE, FALSE, 0);
	
	gtk_widget_show(mbot->psfBox);
	return;
}


void gtk_fieldline_menu_box(struct main_buttons *mbot, GtkWidget *window){
	GtkWidget *closeButton;
	
	GtkWidget *entry_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_file), "parent", (gpointer) window);
	g_object_set_data(G_OBJECT(entry_file), "buttons", (gpointer) mbot);
		
	closeButton = gtk_button_new_with_label("Close Current PSF");
	g_signal_connect(G_OBJECT(closeButton), "clicked", 
				G_CALLBACK(close_fline_CB), entry_file);
	
	mbot->fline_menu = (struct fieldline_gtk_menu *) malloc(sizeof(struct fieldline_gtk_menu));
	GtkWidget *menu_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_box_pack_start(GTK_BOX(menu_box), closeButton, FALSE, FALSE, 0);
	GtkWidget *hbox = init_fieldline_menu_hbox(mbot->fline_menu, menu_box);
	set_gtk_fieldline_menu(mbot->fline_menu);
    gtk_box_pack_start(GTK_BOX(mbot->flineBox), hbox, FALSE, FALSE, 0);
	
	gtk_widget_show(mbot->flineBox);
	
	return;
}

void gtk_mesh_menu_box(struct main_buttons *mbot, GtkWidget *window){
	GtkWidget *entry_file;
	GtkWidget *closeMeshButton;
	
	
    init_mesh_views_4_viewer(mbot->mesh_vws);
	
	/*  Set buttons */
	entry_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_file), "parent", (gpointer) window);
	g_object_set_data(G_OBJECT(entry_file), "buttons", (gpointer) mbot);
	
	closeMeshButton = gtk_button_new_with_label("Close mesh");
	g_signal_connect(G_OBJECT(closeMeshButton), "clicked", 
				G_CALLBACK(close_mesh_CB), (gpointer) entry_file);
	
	GtkWidget *vbox_mesh = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(vbox_mesh), closeMeshButton, FALSE, FALSE, 0);
	add_gtk_mesh_menu(mbot->mesh_vws, window, vbox_mesh);
	
	GtkWidget *hbox = wrap_into_frame_gtk("Mesh", vbox_mesh);
    gtk_box_pack_start(GTK_BOX(mbot->meshBox), hbox, FALSE, FALSE, 0);
	
	return;
}

void make_gtk_main_menu_box(struct main_buttons *mbot, GtkWidget *window_main){	
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
				G_CALLBACK(set_viewtype_CB), entry_file);
	
	
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
	
    GtkWidget *hbox_axis = make_axis_menu_box();
    GtkWidget *expander_rot = init_rotation_menu_expander(mbot->rot_gmenu, window_main);
    GtkWidget *expander_view = init_viewmatrix_menu_expander(mbot->view_menu, window_main);
    GtkWidget *expander_pref = init_preference_expander(mbot->pref_gmenu, window_main);

    
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), hbox_open, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), hbox_image_save, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), hbox_viewtype, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), hbox_axis, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), expander_rot, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), expander_view, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(mbot->vbox_menu), expander_pref, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(mbot->menuHbox), mbot->vbox_menu, FALSE, FALSE, 0);

    gtk_widget_show_all(expander_pref);
	gtk_widget_show_all(mbot->vbox_menu);
	
	update_kemoview_menu(mbot, window_main);
}
