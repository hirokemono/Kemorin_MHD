/*
 *  kemoview_gtk_fileselector.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_fileselector.h"

GtkWidget *filew;
GtkWidget *fmtw;
GtkWidget *ftmpw_f;
static const gchar *gtk_selected_filename;
static int iflag_set;

/*
   Constract input windows
*/

int kemoview_gtk_read_file_select(GtkButton *button, gpointer entry_data){
	int response;
	GtkEntry *entry = GTK_ENTRY(entry_data);
    GtkWidget *parent = GTK_WIDGET(g_object_get_data(G_OBJECT(entry_data), "parent"));
	GtkFileChooser *chooser;
	
	GtkFileChooserAction action = GTK_FILE_CHOOSER_ACTION_OPEN;

    iflag_set = IZERO;
	/* generate file selection widget*/
	
	filew = gtk_file_chooser_dialog_new("Select File", GTK_WINDOW(parent), action,
				"_Cancel", GTK_RESPONSE_CANCEL, "_Open", GTK_RESPONSE_ACCEPT, NULL);
	
	gtk_widget_show_all(filew);
	
	response = gtk_dialog_run(GTK_DIALOG(filew));
	if (response == GTK_RESPONSE_ACCEPT){
		chooser = GTK_FILE_CHOOSER (filew);
		gtk_selected_filename = gtk_file_chooser_get_filename (chooser);
        g_print ("file name on Menu: %s\n", gtk_selected_filename);
		gtk_entry_set_text(entry, gtk_selected_filename);
		iflag_set = IONE;
	}
	else if( response == GTK_RESPONSE_CANCEL ){
		g_print( "Cancel button was pressed.\n" );
		iflag_set = ZERO;
	}
	else{
    g_print( "Another response was received.\n" );
	}
 	gtk_widget_destroy(filew);
	return iflag_set;
}

int kemoview_gtk_save_file_select(GtkButton *button, gpointer data){
	int response;
	GtkWidget *parent;
	GtkEntry *entry = GTK_ENTRY(data);
	GtkFileChooser *chooser;
	
	GtkFileChooserAction action = GTK_FILE_CHOOSER_ACTION_SAVE;
	
	parent = GTK_WIDGET(g_object_get_data(G_OBJECT(data), "parent"));
	entry = GTK_ENTRY(data);
	
	
	iflag_set = IZERO;
	/* generate file selection widget*/
	
	filew = gtk_file_chooser_dialog_new("Save File", GTK_WINDOW(parent), action,
				"_Cancel", GTK_RESPONSE_CANCEL, "_Open", GTK_RESPONSE_ACCEPT, NULL);
	
	gtk_widget_show_all(filew);
	
	response = gtk_dialog_run(GTK_DIALOG(filew));
	if (response == GTK_RESPONSE_ACCEPT){
		chooser = GTK_FILE_CHOOSER (filew);
		gtk_selected_filename = gtk_file_chooser_get_filename (chooser);
		g_print ("%s\n", gtk_selected_filename);
		gtk_entry_set_text(entry, gtk_selected_filename);
		iflag_set = IONE;
	}
	else if( response == GTK_RESPONSE_CANCEL ){
		g_print( "Cancel button was pressed.\n" );
		iflag_set = ZERO;
	}
	else{
		g_print( "Another response was received.\n" );
	}
 	gtk_widget_destroy(filew);
	return iflag_set;
}

struct kv_string * kemoview_read_file_panel(GtkWidget *window_cmap){
	GtkWidget *entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer) window_cmap);
	int iflag_set = kemoview_gtk_read_file_select(NULL, G_OBJECT(entry));
	struct kv_string *filename = kemoview_init_kvstring_by_string(gtk_selected_filename);
	if(iflag_set == IZERO){
		filename->string[0] = '\0';
	};
	return filename;
};
struct kv_string * kemoview_save_file_panel(GtkWidget *window_cmap){
	GtkWidget *entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer) window_cmap);
	int iflag_set = kemoview_gtk_save_file_select(NULL, G_OBJECT(entry));
	struct kv_string *filename = kemoview_init_kvstring_by_string(gtk_selected_filename);
	if(iflag_set == IZERO){
		filename->string[0] = '\0';
	};
	return filename;
};

/*
   GLUT callback routines for file IO
*/

void set_pickup_command_gtk(struct kv_string *filename){
	GtkWidget *hbox;
    GtkWidget *entry;
	
	/*  Set empty window to make file dialog */
	ftmpw_f = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	
	/*  Generate entry  */
	entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer)ftmpw_f);
	
	int iflag_set = kemoview_gtk_read_file_select(NULL, entry);
	
	kemoview_alloc_copy_string(gtk_selected_filename, filename);
	return;
}

