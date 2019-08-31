/*
 *  kemoview_fileselector_gtk.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_fileselector_gtk.h"

GtkWidget *filew;
GtkWidget *fmtw;
GtkWidget *ftmpw_f;
static const gchar *gtk_selected_filename;
static const gchar *gtk_selected_filefmt;
static int iflag_set;
static int gtk_istart, gtk_iend, gtk_inc;
int id_rotation;

static char X_label[1] = "X";
static char Y_label[1] = "Y";
static char Z_label[1] = "Z";

/*
GTK callback routines
*/
static void destroy (GtkWidget *widget, gpointer data)
{
	gtk_main_quit();
};

static void fmt_clicked(GtkWidget *widget, gpointer data)
{
	iflag_set = IONE;
	gtk_widget_destroy(fmtw);
	gtk_main_quit();
}

static void cancel_clicked(GtkWidget *widget, gpointer data)
{
	iflag_set = IZERO;
	gtk_widget_destroy(fmtw);
	gtk_main_quit();
}

static void fmt_clicked2(GtkWidget *widget, gpointer data)
{
	/*iflag_set = IZERO;*/
	gtk_widget_destroy(ftmpw_f);
	gtk_main_quit();
}

static void fmt_changed(GtkWidget *combo, gpointer data)
{
	gtk_selected_filefmt = gtk_combo_box_text_get_active_text(GTK_COMBO_BOX_TEXT(combo));
	
	printf("Format: %s\n", gtk_selected_filefmt);
/*	printf("index: %d\n",idx_menu);*/
	
}

static void StartChange(GtkWidget *entry, gpointer data)
{
	gtk_istart = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
/*	printf("gtk_istart %d\n", gtk_istart);*/
}
static void EndChange(GtkWidget *entry, gpointer data)
{
	gtk_iend = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
/*	printf("gtk_iend %d\n", gtk_iend);*/
}
static void IncChange(GtkWidget *entry, gpointer data)
{
	gtk_inc = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
/*	printf("gtk_inc %d\n", gtk_inc);*/
}

/*
   Constract input windows
*/

int kemoview_gtk_read_file_select(gpointer data){
	int response;
	GtkWidget *parent;
	GtkEntry *entry;
	GtkFileChooser *chooser;
	
	GtkFileChooserAction action = GTK_FILE_CHOOSER_ACTION_OPEN;
	
	parent = GTK_WIDGET(g_object_get_data(G_OBJECT(data), "parent"));
	entry = GTK_ENTRY(data);
	
	
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
	
	filew = gtk_file_chooser_dialog_new("Select File", GTK_WINDOW(parent), action,
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
	int iflag_set = kemoview_gtk_read_file_select(G_OBJECT(entry));
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

static void gtk_read_file_window(const char *title){
	GtkWidget *hbox;
    GtkWidget *entry;
	
	/*  Set empty window to make file dialog */
	ftmpw_f = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	
	/*  Generate entry  */
	entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer)ftmpw_f);
	
	int iflag_set = kemoview_gtk_read_file_select(entry);
	return;
	
}
/*
   GLUT callback routines for file IO
*/

static void set_pickup_command_gtk(struct kv_string *filename){
	gtk_read_file_window("Select pickup surface program");
	kemoview_alloc_copy_string(gtk_selected_filename, filename);
	return;
}

void read_kemoview_data_gtk(){
	int iflag_datatype;
    struct kv_string *filename;
    struct kv_string *file_prefix;
    struct kv_string *stripped_ext;
    struct kv_string *command;
	
	
	gtk_read_file_window("Input data file");
	if(iflag_set == IZERO) return;
    
    filename = kemoview_init_kvstring_by_string(gtk_selected_filename);
	
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
	return;
};

