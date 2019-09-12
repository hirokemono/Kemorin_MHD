/*
//  control_elements_IO_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "control_elements_IO_GTK.h"

const char *label_none =    "None";
/*const char *label_begin = "Begin"; */
/* const char *label_file =    "File"; */

const char input_mode_labels[3][KCHARA_C] = {
    "None",
    "File", 
    "Begin",
};


static void cb_file_block_select(GtkComboBox *combobox_cmap, gpointer data)
{
    int *iflag_block = (int *) data;
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_cmap);
    GtkTreeIter iter;
    
    gchar *row_string;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_cmap);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_mode, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_MATH, iflag_block, -1);
    return;
}

static void cb_file_name_input(GtkEntry *entry, gpointer data)
{
    const char *file_name = (char *) data;
    file_name = gtk_entry_get_text(entry);
    return;
};

static void cb_expander_toggle(GObject *check, gpointer data){
    int *iflag_use = (int *) data;
    
	if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(check)) == TRUE) {
        *iflag_use = 1;
    } else {
        *iflag_use = 0;
    };
};

void cb_expander_action(GObject *switch_3, gpointer data){
	GtkWidget *expender = GTK_WIDGET(switch_3);
    int *iflag = (int *) data;
	
	if(*iflag == 0){
		gtk_expander_set_expanded(GTK_EXPANDER(expender), TRUE);
    };
};

void cb_toggle_switch(GtkCheckButton *check, gpointer data){
	struct entry_and_flag *tbox_flag = (struct entry_and_flag *) data;
	
    if(tbox_flag->iflag_fix > 0) gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check), TRUE);
	if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(check)) == TRUE) {
        *tbox_flag->iflag = 1;
        gtk_widget_set_opacity(GTK_WIDGET(tbox_flag->entry), 1.0);
    } else{
        *tbox_flag->iflag = 0;
        gtk_widget_set_opacity(GTK_WIDGET(tbox_flag->entry), 0.2);
	};
	return;
}

void cb_toggle_entry(GtkCheckButton *check, gpointer data)
{
	struct entry_and_flag *tbox_flag = (struct entry_and_flag *) data;
	
    if(tbox_flag->iflag_fix > 0) gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check), TRUE);
	if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(check)) == TRUE) {
        *tbox_flag->iflag = 1;
        gtk_editable_set_editable(GTK_EDITABLE(tbox_flag->entry), TRUE);
        gtk_widget_set_opacity(GTK_WIDGET(tbox_flag->entry), 1.0);
    } else{
        *tbox_flag->iflag = 0;
        gtk_editable_set_editable(GTK_EDITABLE(tbox_flag->entry), FALSE);
        gtk_widget_set_opacity(GTK_WIDGET(tbox_flag->entry), 0.2);
	};
	return;
}

static GtkWidget *make_block_switch_vbox(const char *label_hd, int *iflag_use){
    GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 6);
	GtkWidget *check = gtk_check_button_new_with_label(label_hd);
	
	if(*iflag_use == 0){
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check), FALSE);
	} else {
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check), TRUE);
	};
	g_signal_connect(G_OBJECT(check), "toggled", G_CALLBACK(cb_expander_toggle), 
				(gpointer) iflag_use);
	
    gtk_box_set_baseline_position(GTK_BOX(vbox), GTK_BASELINE_POSITION_TOP);
	gtk_box_pack_start(GTK_BOX(vbox), check, FALSE, FALSE, 0);
	
	return vbox;
};

static GtkWidget *make_control_file_block_vbox(const char *label_hd, int *iflag_use_file, 
			char *file_name, GtkWidget *save_bottun){
	GtkWidget *combo_b;
    GtkWidget *label_tree;
    GtkTreeModel *model;
	GtkTreeModel *child_model;
	GtkCellRenderer *renderer;
	GtkWidget *entry_3 = gtk_entry_new();
    GtkWidget *hbox_y = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
    GtkWidget *hbox_z = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
    GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);

    int index = 0;
	
	index = 0;
	label_tree = create_fixed_label_w_index_tree();
	model = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree));  
	child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
	index = append_ci_item_to_tree(index, &input_mode_labels[0][0], NONE_MODE, child_model);
	index = append_ci_item_to_tree(index, &input_mode_labels[1][0], FILE_MODE, child_model);
	index = append_ci_item_to_tree(index, &input_mode_labels[2][0], TYPE_MODE, child_model);
	
	
	combo_b = gtk_combo_box_new_with_model(child_model);
	renderer = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combo_b), renderer, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combo_b), renderer,
				"text", COLUMN_FIELD_NAME, NULL);
	
	if(*iflag_use_file == TYPE_MODE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combo_b), 2);
	} else if(*iflag_use_file == FILE_MODE){
		gtk_combo_box_set_active(GTK_COMBO_BOX(combo_b), 1);
	} else {
		gtk_combo_box_set_active(GTK_COMBO_BOX(combo_b), 0);
	};
	g_signal_connect(G_OBJECT(combo_b), "changed", G_CALLBACK(cb_file_block_select),
				(gpointer) iflag_use_file);
	
	gtk_entry_set_text(GTK_ENTRY(entry_3), file_name);
    gtk_entry_set_width_chars(GTK_ENTRY(entry_3), 20);
	g_signal_connect(G_OBJECT(entry_3), "activate", G_CALLBACK(cb_file_name_input), (gpointer) file_name);
	
    gtk_box_pack_start(GTK_BOX(hbox_y), combo_b, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_y), gtk_label_new(label_hd), FALSE, FALSE, 0);
    
	gtk_box_pack_start(GTK_BOX(hbox_z), gtk_label_new("File:"), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_z), entry_3, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_z), save_bottun, FALSE, FALSE, 0);
    
    gtk_box_pack_start(GTK_BOX(vbox), hbox_y, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox), hbox_z, FALSE, FALSE, 0);   
	return vbox;
};

GtkWidget *make_expand_ctl_hbox(const char *label_hd, int *iflag_use, int vsize_scroll,
			GtkWidget *vbox_1){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 1);
	GtkWidget *expander = gtk_expander_new("");
	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    GtkWidget *vbox_2 = make_block_switch_vbox(label_hd, iflag_use);
	
	g_signal_connect(G_OBJECT(expander), "activate", G_CALLBACK(cb_expander_action), 
					(gpointer) iflag_use);
    gtk_expander_set_resize_toplevel(GTK_EXPANDER(expander), TRUE);
	
    gtk_container_set_border_width(GTK_CONTAINER(vbox_1), 10);
	
	gtk_widget_set_size_request(scrolled_window, 450, vsize_scroll);
    gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolled_window), GTK_SHADOW_IN);
	gtk_scrolled_window_set_max_content_height(GTK_SCROLLED_WINDOW(scrolled_window), vsize_scroll);
	gtk_container_set_border_width(GTK_CONTAINER(scrolled_window), 5);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
    
	gtk_container_add(GTK_CONTAINER(scrolled_window), vbox_1);
	gtk_container_add(GTK_CONTAINER(expander), scrolled_window);
	
	gtk_box_pack_start(GTK_BOX(hbox), vbox_2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), expander, TRUE, TRUE, 0);
	return hbox;
};

GtkWidget *make_expand_ctl_file_hbox(const char *label_hd, int *iflag_use_file, char *file_name, 
                                     int vsize_scroll, GtkWidget *vbox_1, GtkWidget *save_bottun){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 1);
	GtkWidget *expander = gtk_expander_new("");
	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	
	GtkWidget *vbox_2 = make_control_file_block_vbox(label_hd, iflag_use_file, 
				file_name, save_bottun);
	
	g_signal_connect(G_OBJECT(expander), "activate", G_CALLBACK(cb_expander_action), 
					(gpointer) iflag_use_file);
	
    gtk_container_set_border_width(GTK_CONTAINER(vbox_1), 5);
	
	gtk_widget_set_size_request(scrolled_window, 300, vsize_scroll);
    gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolled_window), GTK_SHADOW_IN);
	gtk_scrolled_window_set_max_content_height(GTK_SCROLLED_WINDOW(scrolled_window), vsize_scroll);
	gtk_container_set_border_width(GTK_CONTAINER(scrolled_window), 5);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
    
	gtk_container_add(GTK_CONTAINER(scrolled_window), vbox_1);
	gtk_container_add(GTK_CONTAINER(expander), scrolled_window);
	
	gtk_box_pack_start(GTK_BOX(hbox), vbox_2, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), expander, TRUE, TRUE, 0);
	return hbox;
};

GtkWidget *make_empty_ctl_hbox(const char *label_hd, int *iflag_use){
	GtkWidget *hbox = make_expand_ctl_hbox(label_hd, iflag_use, 20, 
				gtk_box_new(GTK_ORIENTATION_VERTICAL, 10));
	return hbox;
}

static void cb_switch_chara(GtkSwitch *switch_3, GParamSpec *pspec, gpointer data){
	struct chara_ctl_item *ctl_item = (struct chara_ctl_item *) data;
	
	if(ctl_item->iflag == 0 && gtk_switch_get_state(switch_3) == TRUE){
		gtk_switch_set_state(switch_3, FALSE);
	};
	
	if(gtk_switch_get_state(switch_3) == TRUE){
		if(cmp_no_case_c(ctl_item->c_tbl, "YES") > 0
					|| cmp_no_case_c(ctl_item->c_tbl, "NO")){
			sprintf(ctl_item->c_tbl, "YES");
		} else {
			sprintf(ctl_item->c_tbl, "ON");
		};
	} else {
		if(cmp_no_case_c(ctl_item->c_tbl, "YES") > 0
					|| cmp_no_case_c(ctl_item->c_tbl, "NO")){
			sprintf(ctl_item->c_tbl, "NO");
		} else {
			sprintf(ctl_item->c_tbl, "OFF");
		};
    };
};

static void cb_toggle_ctl_item(GtkToggleButton *toggle, gpointer data)
{
	struct chara_ctl_item *ctl_item = (struct chara_ctl_item *) data;
    gboolean status = gtk_toggle_button_get_active(toggle);
    
	if(ctl_item->c_tbl != NULL) {
        ctl_item->iflag = 1;
        set_boolean_by_chara_ctl_item((int) status, ctl_item);
		gtk_button_set_label(GTK_BUTTON(toggle), ctl_item->c_tbl);
        printf("New value: %s\n", ctl_item->c_tbl);
    };
    return;
}

GtkWidget *make_entry_with_switch_hbox(int iflag_fix_on, const char *label, int *iflag, 
			struct entry_and_flag *tbox_flag){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	tbox_flag->iflag_fix = iflag_fix_on;
	tbox_flag->iflag = iflag;
	tbox_flag->check = gtk_check_button_new_with_label(label);
	
	if(tbox_flag->iflag_fix > 0) *tbox_flag->iflag = 1;
	if(*tbox_flag->iflag == 0){
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(tbox_flag->check), FALSE);
		gtk_widget_set_opacity(GTK_WIDGET(tbox_flag->entry), 0.2);
	} else {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(tbox_flag->check), TRUE);
		gtk_widget_set_opacity(GTK_WIDGET(tbox_flag->entry), 1.0);
	};
	g_signal_connect(G_OBJECT(tbox_flag->check), "toggled", 
				G_CALLBACK(cb_toggle_switch), (gpointer) tbox_flag);
	
	gtk_box_pack_start(GTK_BOX(hbox), tbox_flag->check, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), tbox_flag->entry, FALSE, FALSE, 0);
	return hbox;
};

GtkWidget *make_entry_with_check_hbox(int iflag_fix_on, const char *label, int *iflag, 
			struct entry_and_flag *tbox_flag){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	tbox_flag->iflag_fix = iflag_fix_on;
	tbox_flag->iflag = iflag;
	tbox_flag->check = gtk_check_button_new_with_label(label);
	
	if(tbox_flag->iflag_fix > 0) *tbox_flag->iflag = 1;
	if(*tbox_flag->iflag == 0){
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(tbox_flag->check), FALSE);
		gtk_editable_set_editable(GTK_EDITABLE(tbox_flag->entry), FALSE);
		gtk_widget_set_opacity(GTK_WIDGET(tbox_flag->entry), 0.2);
	} else {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(tbox_flag->check), TRUE);
		gtk_editable_set_editable(GTK_EDITABLE(tbox_flag->entry), TRUE);
		gtk_widget_set_opacity(GTK_WIDGET(tbox_flag->entry), 1.0);
	};
	g_signal_connect(G_OBJECT(tbox_flag->check), "toggled", 
				G_CALLBACK(cb_toggle_entry), (gpointer) tbox_flag);
	
	gtk_box_pack_start(GTK_BOX(hbox), tbox_flag->check, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), tbox_flag->entry, TRUE, TRUE, 0);
	return hbox;
};

GtkWidget *make_chara_ctl_switch_hbox(int iflag_fix_on, const char *label, struct chara_ctl_item *ctl_item){
	GtkWidget *hbox;
	
    struct entry_and_flag *tbox_flag = (struct entry_and_flag *) malloc(sizeof(struct entry_and_flag));
	
	tbox_flag->entry = gtk_switch_new();
	
	gtk_switch_set_active(GTK_SWITCH(tbox_flag->entry), TRUE);
	if(cmp_no_case_c(ctl_item->c_tbl, "YES") > 0){
		gtk_switch_set_active(GTK_SWITCH(tbox_flag->entry), TRUE);
	} else if(cmp_no_case_c(ctl_item->c_tbl, "ON") > 0){
		gtk_switch_set_active(GTK_SWITCH(tbox_flag->entry), TRUE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(tbox_flag->entry), FALSE);
	}
	g_signal_connect(G_OBJECT(tbox_flag->entry), "notify::active",
				G_CALLBACK(cb_switch_chara), (gpointer) ctl_item);
	
	hbox = make_entry_with_switch_hbox(iflag_fix_on, label, &ctl_item->iflag, tbox_flag);
	return hbox;
};

GtkWidget *make_toggle_hbox (const char *label, struct chara_ctl_item *ctl_item,
			gboolean is_on, gboolean is_sensitive){
	GtkWidget *hbox;
	GtkWidget *toggle;
    int iflag=0;

	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
	gtk_box_pack_start (GTK_BOX (hbox), gtk_label_new(label), FALSE, FALSE, 0);

	toggle = gtk_toggle_button_new ();
    iflag = find_boolean_from_chara_ctl_item(ctl_item);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(toggle), (gboolean) iflag);
    gtk_button_set_label(GTK_BUTTON(toggle), ctl_item->c_tbl);
    g_signal_connect(G_OBJECT(toggle), "toggled", G_CALLBACK(cb_toggle_ctl_item), 
                     (gpointer) ctl_item);

    gtk_box_pack_start (GTK_BOX(hbox), toggle, FALSE, FALSE, 0);
	gtk_widget_set_sensitive(toggle, is_sensitive);
	gtk_widget_show (toggle);
  return hbox;
}


static void cb_chara_ctl_item(GtkEntry *entry, gpointer data)
{
	struct chara_ctl_item *ctl_item = (struct chara_ctl_item *) data;
	
	if(ctl_item->c_tbl != NULL) {
		ctl_item->iflag = 1;
		ctl_item->c_tbl = (char *) gtk_entry_get_text(entry);
	};
	return;
}

GtkWidget *make_text_hbox(int iflag_fix_on, const char *label, struct chara_ctl_item *ctl_item){
	GtkWidget *hbox;
	struct entry_and_flag *tbox_flag = (struct entry_and_flag *) malloc(sizeof(struct entry_and_flag));
	
	tbox_flag->entry = gtk_entry_new();
	gtk_entry_set_text(GTK_ENTRY(tbox_flag->entry), ctl_item->c_tbl);
	g_signal_connect(G_OBJECT(tbox_flag->entry), "activate",
				G_CALLBACK(cb_chara_ctl_item), (gpointer) ctl_item);
	
	hbox = make_entry_with_check_hbox(iflag_fix_on, label, &ctl_item->iflag, tbox_flag);
  return hbox;
}


static void cb_int_ctl_item(GtkSpinButton *spinner, gpointer data)
{
	struct int_ctl_item *ctl_item = (struct int_ctl_item *) data;
	
	if(data != NULL) {
		ctl_item->iflag = 1;
		ctl_item->i_data = gtk_spin_button_get_value_as_int(spinner);
/*		printf("New value: %d\n", ctl_item->i_data); */
	};
	return;
}

GtkWidget *make_integer_hbox(int iflag_fix_on, const char *label, struct int_ctl_item *ctl_item){
	GtkWidget *hbox;
	GtkAdjustment *adjust = gtk_adjustment_new(ctl_item->i_data, 0, 2147483648, 1,
                    100, 21474836);
	
	struct entry_and_flag *tbox_flag = (struct entry_and_flag *) malloc(sizeof(struct entry_and_flag));
	
	tbox_flag->entry = gtk_spin_button_new(adjust, 1, 0);
	g_signal_connect(G_OBJECT(tbox_flag->entry), "value-changed",
				G_CALLBACK(cb_int_ctl_item), (gpointer) ctl_item);
	
	hbox = make_entry_with_check_hbox(iflag_fix_on, label, &ctl_item->iflag, tbox_flag);
	return hbox;
}

static void cb_real_ctl_item(GtkEntry *spinner, gpointer data)
{
	struct real_ctl_item *ctl_item = (struct real_ctl_item *) data;
	
	if(data != NULL) {
		ctl_item->iflag = 1;
		ctl_item->r_data = gtk_spin_button_get_value(GTK_SPIN_BUTTON(spinner));
	};
	return;
}

GtkWidget *make_real_hbox(int iflag_fix_on, const char *label, struct real_ctl_item *ctl_item){
	GtkWidget *hbox;
	GtkAdjustment *adjust = gtk_adjustment_new(ctl_item->r_data,
				-1.0e30, 1.0e30, 0.1, 100, 21474836);
	
	struct entry_and_flag *tbox_flag = (struct entry_and_flag *) malloc(sizeof(struct entry_and_flag));
	
	tbox_flag->entry = gtk_spin_button_new(adjust, 0.1, 8);
	g_signal_connect(G_OBJECT(tbox_flag->entry), "value-changed",
				G_CALLBACK(cb_real_ctl_item), (gpointer) ctl_item);
	
	hbox = make_entry_with_check_hbox(iflag_fix_on, label, &ctl_item->iflag, tbox_flag);
	return hbox;
}

static void cb_SelectFile(GtkButton *button, gpointer data)
{
	GtkWidget *dialog;
	GtkWidget *parent;
	GtkEntry *entry;
	
  /* Four selections for GtkFileChooserAction */
	GtkFileChooserAction action[] = {GTK_FILE_CHOOSER_ACTION_OPEN, GTK_FILE_CHOOSER_ACTION_SAVE,
			GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER, GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER};
	gint response;
	gchar *write_file_name;

	parent = GTK_WIDGET(g_object_get_data(G_OBJECT(data), "parent"));
	entry = GTK_ENTRY(data);
	
	/* generate file selection widget*/
	dialog = gtk_file_chooser_dialog_new("File Chooser Dialog", GTK_WINDOW(parent), action[1],
				"_Cancel", GTK_RESPONSE_CANCEL, "_Save", GTK_RESPONSE_ACCEPT, NULL);
	
	gtk_widget_show_all(dialog);
	
	response = gtk_dialog_run(GTK_DIALOG(dialog));
	if( response == GTK_RESPONSE_ACCEPT ){
		/* Get file name */
		write_file_name = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		g_print( "Write file name: %s\n", write_file_name);
		
		gtk_entry_set_text(entry, write_file_name);
		g_free(write_file_name);
	} else if( response == GTK_RESPONSE_CANCEL ){
		g_print( "Cancel button was pressed.\n" );
	} else{
		g_print( "Another response was received.\n" );
	};
	gtk_widget_destroy(dialog);
}

GtkWidget *make_filename_hbox (const char *label, struct chara_ctl_item *ctl_item){
	GtkWidget *hbox;
	GtkWidget *entry;
    GtkWidget *button_S;

	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
	gtk_box_pack_start (GTK_BOX (hbox), gtk_label_new(label), FALSE, FALSE, 0);
	
	entry = gtk_entry_new();
    gtk_entry_set_text(GTK_ENTRY(entry), ctl_item->c_tbl);
	g_signal_connect(G_OBJECT(entry), "activate", G_CALLBACK(cb_chara_ctl_item), 
				(gpointer) ctl_item);
	gtk_box_pack_start(GTK_BOX(hbox), entry, TRUE, TRUE, 0);
	
	button_S = gtk_button_new_with_label("Select");
	g_signal_connect(G_OBJECT(button_S), "clicked", G_CALLBACK(cb_SelectFile), (gpointer) entry);
	gtk_box_pack_start(GTK_BOX(hbox), button_S, FALSE, FALSE, 0);
	
	return hbox;
}
