/*
 *  kemoview_gtk_evolution_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_evolution_menu.h"

struct evolution_gtk_menu * init_evoluaiton_menu_box(void){
	struct evolution_gtk_menu *evo_gmenu
			= (struct evolution_gtk_menu *)  malloc(sizeof(struct evolution_gtk_menu));
	evo_gmenu->id_fmt_evo = 0;
	
	evo_gmenu->istart_evo = 0;
	evo_gmenu->iend_evo =   0;
	evo_gmenu->inc_evo =    1;
	return evo_gmenu;
};

static void evolution_view_CB(GtkButton *button, gpointer user_data){
	GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct evolution_gtk_menu *evo_gmenu 
			= (struct evolution_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "evolution");
	
	gtk_widget_destroy(window);
	gtk_main_quit();
	
	struct kv_string *image_prefix = kemoview_init_kvstring_by_string("CalypsoView");
	
	write_evolution_views(NO_SAVE_FILE, image_prefix, evo_gmenu->istart_evo, evo_gmenu->iend_evo,
						  evo_gmenu->inc_evo);
	kemoview_free_kvstring(image_prefix);
	return;
};

static void evolution_save_CB(GtkButton *button, gpointer user_data){
	int id_image;
	GtkEntry *entry = GTK_ENTRY(user_data);
	GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct evolution_gtk_menu *evo_gmenu 
			= (struct evolution_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "evolution");
	
	struct kv_string *filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
	struct kv_string *stripped_ext = kemoview_alloc_kvstring();
	struct kv_string *file_prefix = kemoview_alloc_kvstring();
	
	gtk_widget_destroy(window);
	gtk_main_quit();
	
	
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
	write_evolution_views(id_image, file_prefix, evo_gmenu->istart_evo, evo_gmenu->iend_evo,
						  evo_gmenu->inc_evo);
    kemoview_free_kvstring(file_prefix);
	
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

static void set_evo_fileformat_CB(GtkComboBox *combobox_filefmt, gpointer user_data)
{
	struct evolution_gtk_menu *evo_gmenu 
			= (struct evolution_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "evolution");
	evo_gmenu->id_fmt_evo = gtk_selected_combobox_index(combobox_filefmt);
	return;
};



GtkWidget * init_evoluaiton_menu_expander(int istep, GtkWidget *window, struct evolution_gtk_menu *evo_gmenu){
    GtkWidget *expand_evo;
    
	GtkWidget *entry_evo_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry_evo_file), "parent", (gpointer) window);
	g_object_set_data(G_OBJECT(entry_evo_file), "evolution", (gpointer) evo_gmenu);
	
	GtkWidget *label_tree_evo_fileformat = create_fixed_label_w_index_tree();
	GtkTreeModel *model_evo_fileformat = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree_evo_fileformat));  
	GtkTreeModel *child_model_evo_fileformat = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_evo_fileformat));
	
	int index = 0;
	index = append_ci_item_to_tree(index, "No Image", NO_SAVE_FILE, child_model_evo_fileformat);
	index = append_ci_item_to_tree(index, "PNG", SAVE_PNG, child_model_evo_fileformat);
	index = append_ci_item_to_tree(index, "BMP", SAVE_BMP, child_model_evo_fileformat);
	
	GtkCellRenderer *renderer_evo_fileformat = gtk_cell_renderer_text_new();
	evo_gmenu->combobox_evo_fileformat = gtk_combo_box_new_with_model(child_model_evo_fileformat);
	evo_gmenu->id_fmt_evo = NO_SAVE_FILE;
	gtk_combo_box_set_active(GTK_COMBO_BOX(evo_gmenu->combobox_evo_fileformat), 0);
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(evo_gmenu->combobox_evo_fileformat),
							   renderer_evo_fileformat, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(evo_gmenu->combobox_evo_fileformat),
								   renderer_evo_fileformat, "text", COLUMN_FIELD_NAME, NULL);
	g_signal_connect(G_OBJECT(evo_gmenu->combobox_evo_fileformat), "changed", 
				G_CALLBACK(set_evo_fileformat_CB), (gpointer) entry_evo_file);
	
	
	GtkWidget *evoSelect_Button = gtk_button_new_with_label("Select...");
	g_signal_connect(evoSelect_Button, "clicked", G_CALLBACK(kemoview_gtk_save_file_select),
				(gpointer) entry_evo_file);
	
	
	GtkWidget *evoView_Button = gtk_button_new_with_label("View Evolution");
	g_signal_connect(G_OBJECT(evoView_Button), "clicked", 
					 G_CALLBACK(evolution_view_CB), (gpointer)entry_evo_file);
	GtkWidget *evoSave_Button = gtk_button_new_with_label("Save Evolution");
	g_signal_connect(G_OBJECT(evoSave_Button), "clicked", 
					 G_CALLBACK(evolution_save_CB), (gpointer)entry_evo_file);
	
	evo_gmenu->istart_evo = istep;
	evo_gmenu->iend_evo =   istep;
	
	GtkAdjustment *adj_evo_start = gtk_adjustment_new(evo_gmenu->istart_evo, 0, evo_gmenu->istart_evo*1000, 1, 1, 0.0);
	evo_gmenu->spin_evo_start = gtk_spin_button_new(GTK_ADJUSTMENT(adj_evo_start), 0, 0);
	g_signal_connect(evo_gmenu->spin_evo_start, "value-changed", 
					 G_CALLBACK(evo_start_step_CB), (gpointer)entry_evo_file);
	
	GtkAdjustment *adj_evo_end = gtk_adjustment_new(evo_gmenu->iend_evo, 0.00, evo_gmenu->iend_evo*1000, 1, 1, 0.0);
	evo_gmenu->spin_evo_end = gtk_spin_button_new(GTK_ADJUSTMENT(adj_evo_end), 0, 0);
	g_signal_connect(evo_gmenu->spin_evo_end, "value-changed", 
					 G_CALLBACK(evo_end_step_CB), (gpointer)entry_evo_file);
	
	GtkAdjustment *adj_evo_increment = gtk_adjustment_new(evo_gmenu->inc_evo, 0, evo_gmenu->iend_evo*100, 1, 1, 0.0);
	evo_gmenu->spin_evo_increment = gtk_spin_button_new(GTK_ADJUSTMENT(adj_evo_increment), 0, 0);
	g_signal_connect(evo_gmenu->spin_evo_increment, "value-changed", 
					 G_CALLBACK(evo_increment_CB), (gpointer)entry_evo_file);
	
	
	GtkWidget *hbox_evo_start = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_evo_start), gtk_label_new("Start step: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_evo_start), evo_gmenu->spin_evo_start, TRUE, TRUE, 0);
	GtkWidget *hbox_evo_end = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_evo_end), gtk_label_new("End step: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_evo_end), evo_gmenu->spin_evo_end, TRUE, TRUE, 0);
	GtkWidget *hbox_evo_increment = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_evo_increment), gtk_label_new("Increment: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_evo_increment), evo_gmenu->spin_evo_increment, TRUE, TRUE, 0);
	
	GtkWidget *hbox_evo_fileformat = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_evo_fileformat), gtk_label_new("File format: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_evo_fileformat), evo_gmenu->combobox_evo_fileformat, TRUE, TRUE, 0);
	
	GtkWidget *hbox_evo_filename = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_evo_filename), gtk_label_new("Image file: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_evo_filename), entry_evo_file, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_evo_filename), evoSelect_Button, TRUE, TRUE, 0);
	
	GtkWidget *hbox_evo_save = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_evo_save), evoView_Button, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_evo_save), evoSave_Button, TRUE, TRUE, 0);
	
	
    GtkWidget *evo_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(evo_box), hbox_evo_start, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(evo_box), hbox_evo_end, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(evo_box), hbox_evo_increment, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(evo_box), hbox_evo_filename, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(evo_box), hbox_evo_fileformat, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(evo_box), hbox_evo_save, FALSE, TRUE, 0);

    expand_evo = wrap_into_expanded_frame_gtk("Evolution", 360, 240, window, evo_box);
	return expand_evo;
}
