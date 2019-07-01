/*
//  control_elements_IO_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "control_elements_IO_GTK.h"

static void cb_expander_switch(GObject *switch_3, GParamSpec *pspec, gpointer data){
    int *iflag = (int *) data;
    
    if(gtk_switch_get_state(switch_3) == TRUE){
        gtk_switch_set_state(switch_3, TRUE);
        *iflag = 1;
    } else {
        gtk_switch_set_state(switch_3, FALSE);
        *iflag = 0;
    };
};

void cb_expander_action(GObject *switch_3, gpointer data){
    struct GtkWidget *expender = (GtkWidget *) switch_3;
    int *iflag = (int *) data;
	
	if(*iflag == 0){
		gtk_expander_set_expanded(GTK_EXPANDER(expender), TRUE);
    };
};

GtkWidget *make_expand_ctl_hbox(const char *label_hd, int *iflag_use, int vsize_scroll,
			GtkWidget *vbox_1){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 1);
	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 1);
	GtkWidget *expander = gtk_expander_new("");
	GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    GtkWidget *hbox_1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);
	GtkWidget *switch_b = gtk_switch_new();
	
	gtk_switch_set_active(GTK_SWITCH(switch_b), TRUE);
	if(*iflag_use > 0){
		gtk_switch_set_state(GTK_SWITCH(switch_b), TRUE);
	} else {
		gtk_switch_set_state(GTK_SWITCH(switch_b), FALSE);
	};
	g_signal_connect(G_OBJECT(switch_b), "notify::active", G_CALLBACK(cb_expander_switch), 
				(gpointer) iflag_use);
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
	
	gtk_box_pack_start(GTK_BOX(hbox_1), gtk_label_new(label_hd), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1), switch_b, FALSE, FALSE, 0);
	
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

static void cb_switch_chara(GObject    *switch_3,
                        GParamSpec *pspec,
                        gpointer    data){
    struct chara_ctl_item *ctl_item = (struct chara_ctl_item *) data;

    if(gtk_switch_get_state(switch_3) == TRUE){
        gtk_switch_set_state(switch_3, TRUE);
		
		if(cmp_no_case_c(ctl_item->c_tbl, "YES") > 0
					|| cmp_no_case_c(ctl_item->c_tbl, "NO")){
			sprintf(ctl_item->c_tbl, "YES");
		} else {
			sprintf(ctl_item->c_tbl, "ON");
		};
    } else {
        gtk_switch_set_state(switch_3, FALSE);
		
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

GtkWidget *make_chara_ctl_switch_hbox(const char *label, struct chara_ctl_item *ctl_item){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	GtkWidget*switch_0 = gtk_switch_new();
	
	gtk_box_set_homogeneous(hbox, FALSE);
	gtk_switch_set_active(GTK_SWITCH(switch_0), TRUE);
	if(cmp_no_case_c(ctl_item->c_tbl, "YES") > 0){
		gtk_switch_set_state(GTK_SWITCH(switch_0), TRUE);
	} else if(cmp_no_case_c(ctl_item->c_tbl, "ON") > 0){
		gtk_switch_set_state(GTK_SWITCH(switch_0), TRUE);
	} else {
		gtk_switch_set_state(GTK_SWITCH(switch_0), FALSE);
	}
	g_signal_connect(G_OBJECT (switch_0), "notify::active", G_CALLBACK(cb_switch_chara), 
				(gpointer) ctl_item);
	
	gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new(label), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), switch_0, FALSE, FALSE, 0);
	return hbox;
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

GtkWidget *make_text_hbox(const char *label, struct chara_ctl_item *ctl_item){
	GtkWidget *hbox;
	GtkWidget *tbox, *current;

	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
	gtk_box_pack_start (GTK_BOX (hbox), gtk_label_new(label), FALSE, FALSE, 0);
	
	tbox = gtk_entry_new();
    gtk_entry_set_text(tbox, ctl_item->c_tbl);
	g_signal_connect(G_OBJECT(tbox), "activate", G_CALLBACK(cb_chara_ctl_item), 
				(gpointer) ctl_item);
	
	gtk_box_pack_start(GTK_BOX(hbox), tbox, TRUE, TRUE, 0);
	
  return hbox;
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

GtkWidget *make_integer_hbox(const char *label, struct int_ctl_item *ctl_item){
	GtkWidget *hbox;
	GtkWidget *spinner, *current;
	GtkAdjustment *adjust;

	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
	gtk_box_pack_start (GTK_BOX (hbox), gtk_label_new(label), FALSE, FALSE, 0);
	
	adjust = gtk_adjustment_new(ctl_item->i_data, 0, 2147483648, 1,
                    100, 21474836);
	spinner = gtk_spin_button_new(adjust, 1, 0);
	g_signal_connect(G_OBJECT(spinner), "value-changed", G_CALLBACK(cb_int_ctl_item), 
				(gpointer) ctl_item);
	
	gtk_box_pack_start(GTK_BOX(hbox), spinner, TRUE, TRUE, 0);
	
  return hbox;
}

static void cb_real_ctl_item(GtkEntry *spinner, gpointer data)
{
	struct real_ctl_item *ctl_item = (struct chara_ctl_item *) data;
	
	if(data != NULL) {
		ctl_item->iflag = 1;
		ctl_item->r_data = gtk_spin_button_get_value(spinner);
/*		printf("New value: %f\n", ctl_item->r_data); */
	};
	return;
}

GtkWidget *make_real_hbox (const char *label, struct real_ctl_item *ctl_item){
	GtkWidget *hbox;
	GtkWidget *spinner, *current;
	GtkAdjustment *adjust;

	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
	gtk_box_pack_start (GTK_BOX (hbox), gtk_label_new(label), FALSE, FALSE, 0);
	
	adjust = gtk_adjustment_new(ctl_item->r_data, -1.0e30, 1.0e30, 0.1,
                    100, 21474836);
	spinner = gtk_spin_button_new(adjust, 0.1, 8);
	g_signal_connect(G_OBJECT(spinner), "value-changed", G_CALLBACK(cb_real_ctl_item), 
				(gpointer) ctl_item);
	
	gtk_box_pack_start(GTK_BOX(hbox), spinner, TRUE, TRUE, 0);
	
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
	GtkWidget *tbox, *current;
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
