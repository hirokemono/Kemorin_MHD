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

static void rot_changed(GtkWidget *combo, gpointer data)
{
	gchar *gtk_selected_rotaxis;
	gtk_selected_rotaxis = gtk_combo_box_text_get_active_text(GTK_COMBO_BOX_TEXT(combo));
	
	if(gtk_selected_rotaxis[0] == X_label[0]){
		id_rotation = 1;
	} else if(gtk_selected_rotaxis[0] == Y_label[0]){
		id_rotation = 2;
	} else if(gtk_selected_rotaxis[0] == Z_label[0]){
		id_rotation = 3;
	};
	
	printf("rotation: %s\n", gtk_selected_rotaxis);
	printf("id_rotation: %d\n",id_rotation);
	
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

void kemoview_gtk_read_file_select(gpointer data){
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
	return;
}

void kemoview_gtk_save_file_select(gpointer data){
	int response;
	GtkWidget *parent;
	GtkEntry *entry;
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
	return;
}

struct kv_string * kemoview_read_file_panel(GtkWidget *window_cmap){
	GtkWidget *entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer) window_cmap);
	kemoview_gtk_read_file_select(G_OBJECT(entry));
	struct kv_string *filename = kemoview_init_kvstring_by_string(gtk_selected_filename);
	if(iflag_set == IZERO){
		filename->string[0] = '\0';
	};
	return filename;
};
struct kv_string * kemoview_save_file_panel(GtkWidget *window_cmap){
	GtkWidget *entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer) window_cmap);
	kemoview_gtk_save_file_select(G_OBJECT(entry));
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
	
	kemoview_gtk_read_file_select(entry);
	return;
	
}

static void gtk_save_file_window(const char *title){
	GtkWidget *entry;
	
	ftmpw_f = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	
	/*  Generate entry  */
	entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer)ftmpw_f);
	
	kemoview_gtk_save_file_select((gpointer)entry);
	
	return;
	
}

static void gtk_image_format_box(GtkWidget *combox){
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(combox), "No Image");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(combox), "PNG");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(combox), "BMP");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(combox), "EPS");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(combox), "PDF");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(combox), "PS");
	return;
}

static void gtk_rotatin_axis_box(GtkWidget *combox){
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(combox), "X");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(combox), "Y");
	gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(combox), "Z");
	return;
}

static void gtk_image_fmt_menu(){
	GtkWidget *box;
	GtkWidget *box0, *box1, *box5;
	GtkWidget *label_file, *label_fmt;
	GtkWidget *bot1, *bot2, *bot3;
	GtkWidget *combox1;
	
	GtkWidget *entry;
	
	iflag_set = IZERO;
	fmtw = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(fmtw), "Select File Format");

	g_signal_connect(fmtw, "destroy", G_CALLBACK(gtk_main_quit), NULL);
	gtk_container_set_border_width(GTK_CONTAINER(fmtw), 5);

	
	box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_container_add(GTK_CONTAINER(fmtw), box);
	
	box0 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 100);
	gtk_container_add(GTK_CONTAINER(box), box0);
	box1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), box1);
	box5 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 100);
	gtk_container_add(GTK_CONTAINER(box), box5);
	
	/* File name box */
	label_file = gtk_label_new("Image file: ");
	gtk_box_pack_start(GTK_BOX(box0), label_file, FALSE, FALSE, 0);
	
	entry = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(box0), entry, TRUE, TRUE, 0);
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer)ftmpw_f);
	
	/* File format box */
	label_fmt = gtk_label_new("Image format: ");
	gtk_box_pack_start(GTK_BOX(box1), label_fmt, FALSE, FALSE, 0);
	
	combox1 = gtk_combo_box_text_new();
	gtk_image_format_box(combox1);
	gtk_combo_box_set_active(GTK_COMBO_BOX_TEXT(combox1), 0);
	fmt_changed(combox1, NULL);
	gtk_box_pack_start(GTK_BOX(box1), combox1, FALSE, FALSE, 0);
	
	bot1 = gtk_button_new_with_label("Cancel");
	gtk_box_pack_start(GTK_BOX(box5), bot1, FALSE, FALSE, 0);
	bot2 = gtk_button_new_with_label("Select...");
	gtk_box_pack_start(GTK_BOX(box5), bot2, FALSE, FALSE, 0);
	bot3 = gtk_button_new_with_label("Save");
	gtk_box_pack_start(GTK_BOX(box5), bot3, FALSE, FALSE, 0);

	g_signal_connect(combox1, "changed", G_CALLBACK(fmt_changed), NULL);
	g_signal_connect(bot1, "clicked", G_CALLBACK(cancel_clicked), NULL);
	g_signal_connect(bot3, "clicked", G_CALLBACK(fmt_clicked), NULL);
	
	gtk_widget_show_all(fmtw);
	
	gtk_main();

	return;
}

static void gtk_evolution_image_menu(int istep){
	GtkWidget *box;
	GtkWidget *box0, *box1, *box2, *box3, *box4, *box5;
	GtkWidget *spin1, *spin2, *spin3;
	GtkWidget *label_file, *label_fmt;
	GtkWidget *lavel1, *lavel2, *lavel3;
	GtkWidget *bot1, *bot2, *bot3;
	GtkWidget *combox1;
	
	GtkWidget *entry;
	
	iflag_set = IZERO;
	fmtw = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(fmtw), "Select File Format");

	g_signal_connect(fmtw, "destroy", G_CALLBACK(destroy), (gpointer) &fmtw);

	gtk_container_set_border_width(GTK_CONTAINER(fmtw), 5);

	
	box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_container_add(GTK_CONTAINER(fmtw), box);
	
	box0 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 100);
	gtk_container_add(GTK_CONTAINER(box), box0);
	box1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), box1);
	box2 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), box2);
	box3 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), box3);
	box4 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), box4);
	box5 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 100);
	gtk_container_add(GTK_CONTAINER(box), box5);
	
	/* File name box */
	label_file = gtk_label_new("Image file: ");
	gtk_box_pack_start(GTK_BOX(box0), label_file, FALSE, FALSE, 0);
	
	entry = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(box0), entry, TRUE, TRUE, 0);
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer)ftmpw_f);
	
	/* File format box */
	label_fmt = gtk_label_new("Image format");
	gtk_box_pack_start(GTK_BOX(box1), label_fmt, TRUE, TRUE, 0);
	
	combox1 = gtk_combo_box_text_new();
	gtk_image_format_box(combox1);
	gtk_combo_box_set_active(GTK_COMBO_BOX_TEXT(combox1), 0);
	fmt_changed(combox1, NULL);
	gtk_box_pack_start(GTK_BOX(box1), combox1, FALSE, FALSE, 0);
	
	
	lavel1 = gtk_label_new("Start step");
	gtk_box_pack_start(GTK_BOX(box2), lavel1, TRUE, TRUE, 0);
	spin1 = gtk_spin_button_new_with_range(IZERO, istep*1000,IONE);
	StartChange(spin1, NULL);
	gtk_box_pack_start(GTK_BOX(box2), spin1, TRUE, TRUE, 0);
	
	lavel2 = gtk_label_new("End step");
	gtk_box_pack_start(GTK_BOX(box3), lavel2, TRUE, TRUE, 0);
	spin2 = gtk_spin_button_new_with_range(IZERO, istep*1000,IONE);
	EndChange(spin2, NULL);
	gtk_box_pack_start(GTK_BOX(box3), spin2, TRUE, TRUE, 0);
	
	lavel3 = gtk_label_new("Increment");
	gtk_box_pack_start(GTK_BOX(box4), lavel3, TRUE, TRUE, 0);
	spin3 = gtk_spin_button_new_with_range(IONE, istep*1000,IONE);
	IncChange(spin3, NULL);
	gtk_box_pack_start(GTK_BOX(box4), spin3, TRUE, TRUE, 0);
	
	bot1 = gtk_button_new_with_label("Cancel");
	gtk_box_pack_start(GTK_BOX(box5), bot1, FALSE, FALSE, 0);
	bot2 = gtk_button_new_with_label("Select...");
	gtk_box_pack_start(GTK_BOX(box5), bot2, FALSE, FALSE, 0);
	bot3 = gtk_button_new_with_label("Save");
	gtk_box_pack_start(GTK_BOX(box5), bot3, FALSE, FALSE, 0);

	g_signal_connect(combox1, "changed", G_CALLBACK(fmt_changed), NULL);
	g_signal_connect(spin1, "value-changed", G_CALLBACK(StartChange), NULL);
	g_signal_connect(spin2, "value-changed", G_CALLBACK(EndChange), NULL);
	g_signal_connect(spin3, "value-changed", G_CALLBACK(IncChange), NULL);
	g_signal_connect(bot1, "clicked", G_CALLBACK(destroy), NULL);
	g_signal_connect(bot2, "clicked", G_CALLBACK(kemoview_gtk_save_file_select),
				(gpointer)entry);
	g_signal_connect(bot3, "clicked", G_CALLBACK(fmt_clicked), NULL);

	gtk_widget_show_all(fmtw);
	gtk_main();
	return;
}

static void gtk_rotation_image_menu(){
	GtkWidget *box;
	GtkWidget *box0, *box1, *box2, *box4, *box5;
	GtkWidget *spin3;
	GtkWidget *label_file, *label_fmt;
	GtkWidget *label_rot, *lavel3;
	GtkWidget *bot1, *bot2, *bot3;
	GtkWidget *combox1, *combox2;
	
	GtkWidget *entry;
	
	iflag_set = IZERO;
	fmtw = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(fmtw), "Select File Format");

	g_signal_connect(fmtw, "destroy", G_CALLBACK(destroy), &fmtw);

	gtk_container_set_border_width(GTK_CONTAINER(fmtw), 5);

	
	box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_container_add(GTK_CONTAINER(fmtw), box);
	
	box0 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 100);
	gtk_container_add(GTK_CONTAINER(box), box0);
	box1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), box1);
	box2 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), box2);
	box4 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), box4);
	box5 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 100);
	gtk_container_add(GTK_CONTAINER(box), box5);
	
	/* File name box */
	label_file = gtk_label_new("Image file: ");
	gtk_box_pack_start(GTK_BOX(box0), label_file, FALSE, FALSE, 0);
	
	entry = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(box0), entry, TRUE, TRUE, 0);
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer)ftmpw_f);
	
	/* File format box */
	label_fmt = gtk_label_new("Image format");
	gtk_box_pack_start(GTK_BOX(box1), label_fmt, TRUE, TRUE, 0);
	
	combox1 = gtk_combo_box_text_new();
	gtk_image_format_box(combox1);
	gtk_combo_box_set_active(GTK_COMBO_BOX_TEXT(combox1), 0);
	fmt_changed(combox1, NULL);
	gtk_box_pack_start(GTK_BOX(box1), combox1, FALSE, FALSE, 0);
	
	/* Rotatin axis combo box */
	label_rot = gtk_label_new("Rotation axis");
	gtk_box_pack_start(GTK_BOX(box2), label_rot, TRUE, TRUE, 0);
	
	combox2 = gtk_combo_box_text_new();
	gtk_rotatin_axis_box(combox2);
	gtk_combo_box_set_active(GTK_COMBO_BOX_TEXT(combox2), 0);
	rot_changed(combox2, NULL);
	gtk_box_pack_start(GTK_BOX(box2), combox2, FALSE, FALSE, 0);
	
	/* Rotatin increment combo box */
	lavel3 = gtk_label_new("Step (Deg.)");
	gtk_box_pack_start(GTK_BOX(box4), lavel3, TRUE, TRUE, 0);
	spin3 = gtk_spin_button_new_with_range(IONE, 360, IONE);
	IncChange(spin3, NULL);
	gtk_box_pack_start(GTK_BOX(box4), spin3, TRUE, TRUE, 0);
	
	bot1 = gtk_button_new_with_label("Cancel");
	gtk_box_pack_start(GTK_BOX(box5), bot1, FALSE, FALSE, 0);
	bot2 = gtk_button_new_with_label("Select...");
	gtk_box_pack_start(GTK_BOX(box5), bot2, FALSE, FALSE, 0);
	bot3 = gtk_button_new_with_label("Save");
	gtk_box_pack_start(GTK_BOX(box5), bot3, FALSE, FALSE, 0);

	g_signal_connect(combox1, "changed", G_CALLBACK(fmt_changed), NULL);
	g_signal_connect(combox2, "changed", G_CALLBACK(rot_changed), NULL);
	g_signal_connect(spin3, "value-changed", G_CALLBACK(IncChange), NULL);
	g_signal_connect(bot1, "clicked", G_CALLBACK(destroy), NULL);
	g_signal_connect(bot2, "clicked", G_CALLBACK(kemoview_gtk_save_file_select),
				(gpointer)entry);
	g_signal_connect(bot3, "clicked", G_CALLBACK(fmt_clicked), NULL);

	gtk_widget_show_all(fmtw);

	gtk_main();

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


int input_texture_file_gtk(struct kv_string *file_prefix){
    struct kv_string *filename;
    struct kv_string *stripped_ext;
	int id_img;
	
	gtk_read_file_window("Select texture file");
	if(iflag_set == IZERO) return 0;
	
    stripped_ext = kemoview_alloc_kvstring();
    filename = kemoview_init_kvstring_by_string(gtk_selected_filename);
	kemoview_get_ext_from_file_name(filename, file_prefix, stripped_ext);
	
	id_img = kemoview_set_image_file_format_id(stripped_ext);
    kemoview_free_kvstring(stripped_ext);
    kemoview_free_kvstring(filename);
	return id_img;
}

int output_image_file_gtk(struct kv_string *file_prefix){
    struct kv_string *filename;
    struct kv_string *stripped_ext;
	int id_img;
	
	gtk_image_fmt_menu();
	if(iflag_set == IZERO) return 0;
	
    stripped_ext = kemoview_init_kvstring_by_string(gtk_selected_filefmt);
	id_img = kemoview_set_image_file_format_id(stripped_ext);
    kemoview_free_kvstring(stripped_ext);
	if(id_img != 0){
        stripped_ext = kemoview_alloc_kvstring();
        filename =     kemoview_init_kvstring_by_string(gtk_selected_filename);
		kemoview_get_ext_from_file_name(filename, file_prefix, stripped_ext);
        kemoview_free_kvstring(stripped_ext);
        kemoview_free_kvstring(filename);
	};
	
	return id_img;
}

int output_evolution_file_gtk(struct kv_string *file_prefix,
			int *ist_udt, int *ied_udt, int *inc_udt){
    struct kv_string *filename;
    struct kv_string *stripped_ext;
	int id_img;
	
	printf("ist_udt %d \n",*ist_udt);
	gtk_evolution_image_menu(*ist_udt);
	if(iflag_set == IZERO) return 0;
	
	*ist_udt = gtk_istart;
	*ied_udt = gtk_iend;
	*inc_udt = gtk_inc;
    stripped_ext = kemoview_init_kvstring_by_string(gtk_selected_filefmt);
	id_img = kemoview_set_image_file_format_id(stripped_ext);
	
	if(id_img != 0){
        filename =     kemoview_init_kvstring_by_string(gtk_selected_filename);
		kemoview_get_ext_from_file_name(filename, file_prefix, stripped_ext);
        kemoview_free_kvstring(filename);
	};
    kemoview_free_kvstring(stripped_ext);
	
	return id_img;
}

int output_rotation_file_gtk(struct kv_string *file_prefix, int *axis_ID, int *inc_rot){
    struct kv_string *filename;
    struct kv_string *stripped_ext;
	int id_img;
	
	gtk_rotation_image_menu();
	if(iflag_set == IZERO) return 0;
	
	*axis_ID = id_rotation;
	*inc_rot = gtk_inc;
    stripped_ext = kemoview_init_kvstring_by_string(gtk_selected_filefmt);
	id_img = kemoview_set_image_file_format_id(stripped_ext);
	
	if(id_img != 0){
        filename =     kemoview_init_kvstring_by_string(gtk_selected_filename);
		kemoview_get_ext_from_file_name(filename, file_prefix, stripped_ext);
        kemoview_free_kvstring(filename);
	};
    kemoview_free_kvstring(stripped_ext);
	return id_img;
}

void save_viewmatrix_file_gtk(){
    struct kv_string *filename;
	
	gtk_save_file_window("Save view matrix file");
	if(iflag_set == IZERO) return;
	
    filename = kemoview_init_kvstring_by_string(gtk_selected_filename);
	kemoview_write_modelview_file(filename);
    kemoview_free_kvstring(filename);
	return;
};

void load_viewmatrix_file_gtk(){
    struct kv_string *filename;
    
	gtk_read_file_window("Load view matrix file");
	if(iflag_set == IZERO) return;
	
    filename = kemoview_init_kvstring_by_string(gtk_selected_filename);
	kemoview_load_modelview_file(filename);
    kemoview_free_kvstring(filename);
	
	return;
};

