/*
//  control_elements_IO_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "control_elements_IO_GTK.h"

#define NONE_MODE   0
#define FILE_MODE  -1
#define TYPE_MODE   1

struct entry_and_flag{
    int iflag_fix;
    int *iflag;
    GtkWidget *entry;
    GtkWidget *check;
	GtkWidget *hbox;
};

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
    char *file_name = (char *) data;
    file_name = gtk_entry_get_text(entry);
    return;
};

static void cb_expander_switch(GObject *switch_3, GParamSpec *pspec, gpointer data){
    int *iflag_use = (int *) data;
    
    if(gtk_switch_get_active(switch_3) == TRUE){
        *iflag_use = 1;
    } else {
        *iflag_use = 0;
    };
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
    struct GtkWidget *expender = (GtkWidget *) switch_3;
    int *iflag = (int *) data;
	
	if(*iflag == 0){
		gtk_expander_set_expanded(GTK_EXPANDER(expender), TRUE);
    };
};

static GtkWidget *make_block_switch_hbox(const char *label_hd, int *iflag_use){
    GtkWidget *hbox_1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	GtkWidget *check = gtk_check_button_new_with_label(label_hd);
	
	if(*iflag_use == 0){
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check), FALSE);
	} else {
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check), TRUE);
	};
	g_signal_connect(G_OBJECT(check), "toggled", G_CALLBACK(cb_expander_toggle), 
				(gpointer) iflag_use);
	
	gtk_box_pack_start(GTK_BOX(hbox_1), check, FALSE, FALSE, 0);
	
	return hbox_1;
};

static GtkWidget *make_control_file_block_hbox(const char *label_hd, int *iflag_use_file, 
			char *file_name, GtkWidget *save_bottun){
	GtkWidget *combo_b;
    GtkWidget *label_tree;
    GtkTreeModel *model;
	GtkTreeModel *child_model;
	GtkWidget *entry_3 = gtk_entry_new();
    GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);

    int index = 0;
	
	index = 0;
	label_tree = gtk_tree_view_new();
	create_fixed_label_w_index_tree(label_tree);
	model = gtk_tree_view_get_model (label_tree);  
	child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
	index = append_ci_item_to_tree(index, &input_mode_labels[0][0], NONE_MODE, child_model);
	index = append_ci_item_to_tree(index, &input_mode_labels[1][0], FILE_MODE, child_model);
	index = append_ci_item_to_tree(index, &input_mode_labels[2][0], TYPE_MODE, child_model);
	
	
	combo_b = gtk_combo_box_new_with_model(child_model);
	child_model = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combo_b), child_model, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combo_b), child_model,
				"text", COLUMN_FIELD_NAME, NULL);
	gtk_combo_box_set_active(combo_b, iflag_use_file);
	g_signal_connect(G_OBJECT(combo_b), "changed", G_CALLBACK(cb_file_block_select),
				(gpointer) iflag_use_file);
	
	gtk_entry_set_text(entry_3, file_name);
	g_signal_connect(G_OBJECT(entry_3), "activate", G_CALLBACK(cb_file_name_input), (gpointer) file_name);
	
	gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(label_hd), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), combo_b, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new("File:"), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), entry_3, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), save_bottun, FALSE, FALSE, 0);
	return hbox;
};

GtkWidget *make_expand_ctl_hbox(const char *label_hd, int *iflag_use, int vsize_scroll,
			GtkWidget *vbox_1){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 1);
	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 1);
	GtkWidget *expander = gtk_expander_new("");
	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    GtkWidget *hbox_1 = make_block_switch_hbox(label_hd, iflag_use);
	
	g_signal_connect(G_OBJECT(expander), "activate", G_CALLBACK(cb_expander_action), 
					(gpointer) iflag_use);
	
    gtk_container_set_border_width(GTK_CONTAINER(vbox_1), 5);
	
	gtk_widget_set_size_request(scrolled_window, 300, vsize_scroll);
    gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolled_window), GTK_SHADOW_IN);
	gtk_scrolled_window_set_max_content_height(scrolled_window, vsize_scroll);
	gtk_container_set_border_width(GTK_CONTAINER(scrolled_window), 5);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
    
	gtk_scrolled_window_add_with_viewport(
		GTK_SCROLLED_WINDOW(scrolled_window), vbox_1);
	
	gtk_container_add(GTK_CONTAINER(expander), scrolled_window);
	
	gtk_box_pack_start(GTK_BOX(vbox), hbox_1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), expander, TRUE, TRUE, 0);
	
	gtk_box_pack_start(GTK_BOX(hbox), vbox, TRUE, TRUE, 0);
	return hbox;
};

GtkWidget *make_expand_ctl_file_hbox(const char *label_hd, int *iflag_use_file, char *file_name, 
                                     int vsize_scroll, GtkWidget *vbox_1, GtkWidget *save_bottun){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 1);
	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 1);
	GtkWidget *expander = gtk_expander_new("");
	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	
	GtkWidget *hbox_1 = make_control_file_block_hbox(label_hd, iflag_use_file, 
				file_name, save_bottun);
	
	g_signal_connect(G_OBJECT(expander), "activate", G_CALLBACK(cb_expander_action), 
					(gpointer) iflag_use_file);
	
    gtk_container_set_border_width(GTK_CONTAINER(vbox_1), 5);
	
	gtk_widget_set_size_request(scrolled_window, 300, vsize_scroll);
    gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolled_window), GTK_SHADOW_IN);
	gtk_scrolled_window_set_max_content_height(scrolled_window, vsize_scroll);
	gtk_container_set_border_width(GTK_CONTAINER(scrolled_window), 5);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);
    
	gtk_scrolled_window_add_with_viewport(
		GTK_SCROLLED_WINDOW(scrolled_window), vbox_1);
	
	gtk_container_add(GTK_CONTAINER(expander), scrolled_window);
	
	gtk_box_pack_start(GTK_BOX(vbox), hbox_1, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), expander, TRUE, TRUE, 0);
	
	gtk_box_pack_start(GTK_BOX(hbox), vbox, TRUE, TRUE, 0);
	return hbox;
};

GtkWidget *make_empty_ctl_hbox(const char *label_hd, int *iflag_use){
	GtkWidget *hbox = make_expand_ctl_hbox(label_hd, iflag_use, 20, 
				gtk_box_new(GTK_ORIENTATION_VERTICAL, 10));
	return hbox;
}

static void cb_switch_chara(GObject *switch_3, GParamSpec *pspec, gpointer data){
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
		gtk_button_set_label(toggle, ctl_item->c_tbl);
        printf("New value: %s\n", ctl_item->c_tbl);
    };
    return;
}

static void cb_toggle_switch(GtkCheckButton *check, gpointer data){
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

static void cb_toggle_entry(GtkCheckButton *check, gpointer data)
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

GtkWidget *make_chara_ctl_switch_hbox(int iflag_fix_on, const char *label, struct chara_ctl_item *ctl_item){
    struct entry_and_flag *tbox_flag = (struct entry_and_flag *) malloc(sizeof(struct entry_and_flag));
	
    tbox_flag->iflag_fix = iflag_fix_on;
    tbox_flag->iflag = &ctl_item->iflag;
	tbox_flag->entry = gtk_switch_new();
	tbox_flag->check = gtk_check_button_new_with_label(label);
	tbox_flag->hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
	gtk_box_set_homogeneous(tbox_flag->hbox, FALSE);
	gtk_switch_set_active(GTK_SWITCH(tbox_flag->entry), TRUE);
	if(cmp_no_case_c(ctl_item->c_tbl, "YES") > 0){
		gtk_switch_set_active(GTK_SWITCH(tbox_flag->entry), TRUE);
	} else if(cmp_no_case_c(ctl_item->c_tbl, "ON") > 0){
		gtk_switch_set_active(GTK_SWITCH(tbox_flag->entry), TRUE);
	} else {
		gtk_switch_set_active(GTK_SWITCH(tbox_flag->entry), FALSE);
	}
	
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
	g_signal_connect(G_OBJECT(tbox_flag->entry), "notify::active",
				G_CALLBACK(cb_switch_chara), (gpointer) ctl_item);
	
	gtk_box_pack_start(GTK_BOX(tbox_flag->hbox), tbox_flag->check, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(tbox_flag->hbox), tbox_flag->entry, FALSE, FALSE, 0);
	return tbox_flag->hbox;
};

GtkWidget *make_toggle_hbox (const char *label, struct chara_ctl_item *ctl_item,
			gboolean is_on, gboolean is_sensitive){
	GtkWidget *hbox;
	GtkToggleButton *toggle;
    int iflag=0;

	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
	gtk_box_pack_start (GTK_BOX (hbox), gtk_label_new(label), FALSE, FALSE, 0);

	toggle = gtk_toggle_button_new ();
    iflag = find_boolean_from_chara_ctl_item(ctl_item);
    gtk_toggle_button_set_active(toggle, (gboolean) iflag);
    gtk_button_set_label(toggle, ctl_item->c_tbl);
    g_signal_connect(G_OBJECT(toggle), "toggled", G_CALLBACK(cb_toggle_ctl_item), 
                     (gpointer) ctl_item);

    gtk_box_pack_start (GTK_BOX (hbox), toggle, FALSE, FALSE, 0);
	gtk_widget_set_sensitive (toggle, is_sensitive);
	gtk_widget_show (toggle);
  return hbox;
}


static void cb_chara_ctl_item(GtkEntry *entry, gpointer data)
{
	struct chara_ctl_item *ctl_item = (struct chara_ctl_item *) data;
	
	if(ctl_item->c_tbl != NULL) {
		ctl_item->iflag = 1;
		ctl_item->c_tbl = gtk_entry_get_text(entry);
	};
	return;
}

GtkWidget *make_text_hbox(int iflag_fix_on, const char *label, struct chara_ctl_item *ctl_item){
    struct entry_and_flag *tbox_flag = (struct entry_and_flag *) malloc(sizeof(struct entry_and_flag));
	
    tbox_flag->iflag_fix = iflag_fix_on;
    tbox_flag->iflag = &ctl_item->iflag;
    tbox_flag->entry = gtk_entry_new();
	tbox_flag->check = gtk_check_button_new_with_label(label);
	tbox_flag->hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
    
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
	g_signal_connect(G_OBJECT(tbox_flag->entry), "activate",
				G_CALLBACK(cb_chara_ctl_item), (gpointer) ctl_item);
	
	gtk_box_pack_start (GTK_BOX(tbox_flag->hbox), tbox_flag->check, FALSE, FALSE, 0);
	
    gtk_entry_set_text(tbox_flag->entry, ctl_item->c_tbl);
	gtk_box_pack_start(GTK_BOX(tbox_flag->hbox), tbox_flag->entry, TRUE, TRUE, 0);
  return tbox_flag->hbox;
}


static void cb_int_ctl_item(GtkEntry *spinner, gpointer data)
{
	struct int_ctl_item *ctl_item = (struct chara_ctl_item *) data;
	
	if(data != NULL) {
		ctl_item->iflag = 1;
		ctl_item->i_data = gtk_spin_button_get_value_as_int(spinner);
/*		printf("New value: %d\n", ctl_item->i_data); */
	};
	return;
}

GtkWidget *make_integer_hbox(int iflag_fix_on, const char *label, struct int_ctl_item *ctl_item){
    struct entry_and_flag *tbox_flag = (struct entry_and_flag *) malloc(sizeof(struct entry_and_flag));
	GtkAdjustment *adjust = gtk_adjustment_new(ctl_item->i_data, 0, 2147483648, 1,
                    100, 21474836);
	
    tbox_flag->iflag_fix = iflag_fix_on;
    tbox_flag->iflag = &ctl_item->iflag;
	tbox_flag->entry = gtk_spin_button_new(adjust, 1, 0);
	tbox_flag->check = gtk_check_button_new_with_label(label);
	tbox_flag->hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
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
	g_signal_connect(G_OBJECT(tbox_flag->entry), "value-changed",
				G_CALLBACK(cb_int_ctl_item), (gpointer) ctl_item);
	
	gtk_box_pack_start(GTK_BOX (tbox_flag->hbox), tbox_flag->check, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(tbox_flag->hbox), tbox_flag->entry, TRUE, TRUE, 0);
	return tbox_flag->hbox;
}

static void cb_real_ctl_item(GtkEntry *spinner, gpointer data)
{
	struct real_ctl_item *ctl_item = (struct chara_ctl_item *) data;
	
	if(data != NULL) {
		ctl_item->iflag = 1;
		ctl_item->r_data = gtk_spin_button_get_value(spinner);
	};
	return;
}

GtkWidget *make_real_hbox(int iflag_fix_on, const char *label, struct real_ctl_item *ctl_item){
    struct entry_and_flag *tbox_flag = (struct entry_and_flag *) malloc(sizeof(struct entry_and_flag));
	GtkAdjustment *adjust = gtk_adjustment_new(ctl_item->r_data,
				-1.0e30, 1.0e30, 0.1, 100, 21474836);
	
    tbox_flag->iflag_fix = iflag_fix_on;
    tbox_flag->iflag = &ctl_item->iflag;
	tbox_flag->entry = gtk_spin_button_new(adjust, 0.1, 8);
	tbox_flag->check = gtk_check_button_new_with_label(label);
	tbox_flag->hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	
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
	g_signal_connect(G_OBJECT(tbox_flag->entry), "value-changed",
				G_CALLBACK(cb_real_ctl_item), (gpointer) ctl_item);
	
	gtk_box_pack_start(GTK_BOX (tbox_flag->hbox), tbox_flag->check, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(tbox_flag->hbox), tbox_flag->entry, TRUE, TRUE, 0);
	return tbox_flag->hbox;
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
	GtkWidget *tbox;
    GtkWidget *button_S;

	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
	gtk_box_pack_start (GTK_BOX (hbox), gtk_label_new(label), FALSE, FALSE, 0);
	
	tbox = gtk_entry_new();
    gtk_entry_set_text(tbox, ctl_item->c_tbl);
	g_signal_connect(G_OBJECT(tbox), "activate", G_CALLBACK(cb_chara_ctl_item), 
				(gpointer) ctl_item);
	gtk_box_pack_start(GTK_BOX(hbox), tbox, TRUE, TRUE, 0);
	
	button_S = gtk_button_new_with_label("Select");
	g_signal_connect(G_OBJECT(button_S), "clicked", G_CALLBACK(cb_SelectFile), (gpointer)tbox);
	gtk_box_pack_start(GTK_BOX(hbox), button_S, FALSE, FALSE, 0);
	
	return hbox;
}
