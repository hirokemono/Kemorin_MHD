/*
//  control_elements_IO_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "control_elements_IO_GTK.h"

static void cb_toggle_ctl_item(GtkEntry *toggle, gpointer data)
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

GtkWidget *make_toggle_hbox (const char *label, struct chara_ctl_item *ctl_item,
			gboolean is_on, gboolean is_sensitive){
	GtkWidget *hbox;
	GtkWidget *toggle, *current;
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

GtkWidget *
make_text_hbox (const char *label, struct chara_ctl_item *ctl_item){
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

GtkWidget *make_integer_hbox (const char *label, struct int_ctl_item *ctl_item){
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
