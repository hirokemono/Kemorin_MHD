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
static const gchar *gtk_selected_filename;
static const gchar *gtk_selected_filefmt;
static int iflag_set;
static int gtk_istart, gtk_iend, gtk_inc;

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

static void fmt_changed(GtkWidget *combo, gpointer data)
{
	gtk_selected_filefmt = gtk_combo_box_get_active_text(GTK_COMBO_BOX(combo));
	
	printf("Format: %s\n",gtk_selected_filefmt);
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

static void file_ok_sel (GtkWidget *w, GtkFileSelection *fs){
	iflag_set = IONE;
	gtk_selected_filename = gtk_file_selection_get_filename (GTK_FILE_SELECTION (fs));
	g_print ("%s\n", gtk_selected_filename);
	
	gtk_widget_destroy(filew);
	gtk_main_quit();
};

/*
   Constract input windows
*/

static void gtk_file_menu(const char *title){
	
	iflag_set = IZERO;
	/* generate file selection widget*/
	filew = gtk_file_selection_new (title);
	gtk_signal_connect (GTK_OBJECT (filew), "destroy",
			(GtkSignalFunc) destroy, &filew);
	/* connect ok_button to file_ok_sel() */
	gtk_signal_connect (GTK_OBJECT (GTK_FILE_SELECTION (filew)->ok_button),
			"clicked", (GtkSignalFunc) file_ok_sel, filew );
	
	/* Connect cancel_button to destroy window */
	gtk_signal_connect_object (GTK_OBJECT (GTK_FILE_SELECTION (filew)->cancel_button),
			"clicked", (GtkSignalFunc) gtk_widget_destroy,
			GTK_OBJECT (filew));
	/* Set default file neme*/
	gtk_file_selection_set_filename (GTK_FILE_SELECTION(filew), "in.0");
	
	gtk_widget_show(filew);
	gtk_main();
}

static void gtk_image_format_box(GtkWidget *c1){
	gtk_combo_box_append_text(GTK_COMBO_BOX(c1), "No Image");
	gtk_combo_box_append_text(GTK_COMBO_BOX(c1), "PNG");
	gtk_combo_box_append_text(GTK_COMBO_BOX(c1), "BMP");
	gtk_combo_box_append_text(GTK_COMBO_BOX(c1), "EPS");
	gtk_combo_box_append_text(GTK_COMBO_BOX(c1), "PDF");
	gtk_combo_box_append_text(GTK_COMBO_BOX(c1), "PS");
	return;
}

static void gtk_image_fmt_menu(){
	GtkWidget *box;
	GtkWidget *box1, *box5;
	GtkWidget *lavel0;
	GtkWidget *bot1, *bot2;
	GtkWidget *c1;
	
	iflag_set = IZERO;
	fmtw = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(fmtw), "Select File Format");

	g_signal_connect(fmtw, "destroy", G_CALLBACK(gtk_main_quit), NULL);

	gtk_container_set_border_width(GTK_CONTAINER(fmtw), 5);

	
	box = gtk_vbox_new(FALSE, 10);
	gtk_container_add(GTK_CONTAINER(fmtw), box);
	
	box1 = gtk_hbox_new(FALSE, 5);
	gtk_container_add(GTK_CONTAINER(box), box1);
	box5 = gtk_hbox_new(FALSE, 100);
	gtk_container_add(GTK_CONTAINER(box), box5);
	
	
	lavel0 = gtk_label_new("Image format");
	gtk_box_pack_start(GTK_BOX(box1), lavel0, TRUE, TRUE, 0);
	
	c1 = gtk_combo_box_new_text();
	gtk_image_format_box(c1);
	gtk_combo_box_set_active(GTK_COMBO_BOX(c1), 0);
	gtk_box_pack_start(GTK_BOX(box1), c1, FALSE, FALSE, 0);
	
	bot1 = gtk_button_new_with_label("Cancel");
	gtk_box_pack_start(GTK_BOX(box5), bot1, FALSE, FALSE, 0);
	bot2 = gtk_button_new_with_label("Save");
	gtk_box_pack_start(GTK_BOX(box5), bot2, FALSE, FALSE, 0);

	g_signal_connect(c1, "changed", G_CALLBACK(fmt_changed), NULL);
	g_signal_connect(bot1, "clicked", G_CALLBACK(destroy), NULL);
	g_signal_connect(bot2, "clicked", G_CALLBACK(fmt_clicked), NULL);
	
	gtk_widget_show_all(fmtw);
	
	gtk_main();

	return;
}

static void gtk_evolution_fmt_menu(int istep){
	GtkWidget *box;
	GtkWidget *box1, *box2, *box3, *box4, *box5;
	GtkWidget *spin1, *spin2, *spin3;
	GtkWidget *lavel0, *lavel1, *lavel2, *lavel3;
	GtkWidget *bot1, *bot2;
	GtkWidget *c1;
	
	iflag_set = IZERO;
	fmtw = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(fmtw), "Select File Format");

	gtk_signal_connect (GTK_OBJECT (fmtw), "destroy",
			(GtkSignalFunc) destroy, &fmtw);

	gtk_container_set_border_width(GTK_CONTAINER(fmtw), 5);

	
	box = gtk_vbox_new(FALSE, 10);
	gtk_container_add(GTK_CONTAINER(fmtw), box);
	
	box1 = gtk_hbox_new(FALSE, 5);
	gtk_container_add(GTK_CONTAINER(box), box1);
	box2 = gtk_hbox_new(FALSE, 5);
	gtk_container_add(GTK_CONTAINER(box), box2);
	box3 = gtk_hbox_new(FALSE, 5);
	gtk_container_add(GTK_CONTAINER(box), box3);
	box4 = gtk_hbox_new(FALSE, 5);
	gtk_container_add(GTK_CONTAINER(box), box4);
	box5 = gtk_hbox_new(FALSE, 100);
	gtk_container_add(GTK_CONTAINER(box), box5);
	
	
	lavel0 = gtk_label_new("Image format");
	gtk_box_pack_start(GTK_BOX(box1), lavel0, TRUE, TRUE, 0);
	
	c1 = gtk_combo_box_new_text();
	gtk_image_format_box(c1);
	gtk_combo_box_set_active(GTK_COMBO_BOX(c1), 0);
	gtk_box_pack_start(GTK_BOX(box1), c1, FALSE, FALSE, 0);
	
	
	lavel1 = gtk_label_new("Start step");
	gtk_box_pack_start(GTK_BOX(box2), lavel1, TRUE, TRUE, 0);
	spin1 = gtk_spin_button_new_with_range(IZERO, istep*1000,IONE);
	gtk_box_pack_start(GTK_BOX(box2), spin1, TRUE, TRUE, 0);
	
	lavel2 = gtk_label_new("End step");
	gtk_box_pack_start(GTK_BOX(box3), lavel2, TRUE, TRUE, 0);
	spin2 = gtk_spin_button_new_with_range(IZERO, istep*1000,IONE);
	gtk_box_pack_start(GTK_BOX(box3), spin2, TRUE, TRUE, 0);
	
	lavel3 = gtk_label_new("Increment");
	gtk_box_pack_start(GTK_BOX(box4), lavel3, TRUE, TRUE, 0);
	spin3 = gtk_spin_button_new_with_range(IONE, istep*1000,IONE);
	gtk_box_pack_start(GTK_BOX(box4), spin3, TRUE, TRUE, 0);
	
	bot1 = gtk_button_new_with_label("Cancel");
	gtk_box_pack_start(GTK_BOX(box5), bot1, FALSE, FALSE, 0);
	bot2 = gtk_button_new_with_label("Save");
	gtk_box_pack_start(GTK_BOX(box5), bot2, FALSE, FALSE, 0);

	g_signal_connect(c1, "changed", G_CALLBACK(fmt_changed), NULL);
	g_signal_connect(spin1, "value-changed", G_CALLBACK(StartChange), NULL);
	g_signal_connect(spin2, "value-changed", G_CALLBACK(EndChange), NULL);
	g_signal_connect(spin3, "value-changed", G_CALLBACK(IncChange), NULL);
	g_signal_connect(bot1, "clicked", G_CALLBACK(destroy), NULL);
	g_signal_connect(bot2, "clicked", G_CALLBACK(fmt_clicked), NULL);

	gtk_widget_show_all(fmtw);

	gtk_main();

	return;
}

/*
   GLUT callback routines for file IO
*/

static void set_pickup_command(char *file_name){
	gtk_file_menu("Select pickup surface program");
	strcpy(file_name, gtk_selected_filename);
	return;
}

void read_kemoview_data_gtk(){
	char file_name[LENGTHBUF];
	char file_head[LENGTHBUF];
	char file_head2[LENGTHBUF];
	char file_ext[LENGTHBUF];
	char pick_command[LENGTHBUF];
	int ierr;
	
	
	gtk_file_menu("Input data file");
	if(iflag_set == IZERO) return;
	
	strcpy(file_name, gtk_selected_filename);
	
	get_ext_from_file_name(file_name, file_head, file_ext);
	printf("file name: %s\n", file_name);
	printf("file_head %s\n", file_head);
	printf("file_ext %s\n", file_ext);
	
	if (		  (file_ext[0] == 'g' && file_ext[1] == 'z')
		||	  (file_ext[0] == 'G' && file_ext[1] == 'Z') ){
		get_ext_from_file_name(file_head, file_head2, file_ext);
		
		if (file_ext[0] == '0' && file_ext[1] == '\0') {
			return;
		}
	} else if (file_ext[0] == '0' && file_ext[1] == '\0') {
		set_pickup_command(pick_command);
		if(iflag_set == IZERO) return;
		set_to_pick_surface_command(pick_command);
	}
	
	ierr = kemoview_open_data_glut(file_name);
	return;
};


int input_texture_file_gtk(char *file_head){
	char file_name[LENGTHBUF];
	char file_ext[LENGTHBUF];
	int id_img;
	
	gtk_file_menu("Select texture file");
	if(iflag_set == IZERO) return 0;
	
	strcpy(file_name, gtk_selected_filename);
	get_ext_from_file_name(file_name, file_head, file_ext);
	
	id_img = set_image_file_format_id(file_ext);
	return id_img;
}

int output_image_file_gtk(char *file_head){
	char image_fmt[LENGTHBUF];
	char file_name[LENGTHBUF];
	char file_ext[LENGTHBUF];
	int id_img;
	
	gtk_image_fmt_menu();
	if(iflag_set == IZERO) return 0;
	
	strcpy(image_fmt, gtk_selected_filefmt);
	id_img = set_image_file_format_id(image_fmt);
	
	if(id_img != 0){
		gtk_file_menu("Save Image file");
		if(iflag_set == IZERO) return 0;
		
		strcpy(file_name, gtk_selected_filename);
		get_ext_from_file_name(file_name, file_head, file_ext);
	};
	
	return id_img;
}

int output_evolution_file_gtk(char *file_head,
			int *ist_udt, int *ied_udt, int *inc_udt){
	char image_fmt[LENGTHBUF];
	char file_name[LENGTHBUF];
	char file_ext[LENGTHBUF];
	int id_img;
	
	printf("ist_udt %d \n",*ist_udt);
	gtk_evolution_fmt_menu(*ist_udt);
	if(iflag_set == IZERO) return 0;
	
	*ist_udt = gtk_istart;
	*ied_udt = gtk_iend;
	*inc_udt = gtk_inc;
	id_img = set_image_file_format_id(image_fmt);
	
	if(id_img != 0){
		gtk_file_menu("Save Image files");
		if(iflag_set == IZERO) return 0;
		
		strcpy(image_fmt, gtk_selected_filefmt);
		strcpy(file_name, gtk_selected_filename);
		get_ext_from_file_name(file_name, file_head, file_ext);
	};
	
	return id_img;
}

void save_viewmatrix_file_gtk(){
	char file_name[LENGTHBUF];
	
	gtk_file_menu("Save view matrix file");
	if(iflag_set == IZERO) return;
	
	strcpy(file_name, gtk_selected_filename);
	write_modelview_file_glut(file_name);
	
	return;
};

void load_viewmatrix_file_gtk(){
	char file_name[LENGTHBUF];
	
	gtk_file_menu("Load view matrix file");
	if(iflag_set == IZERO) return;
	
	strcpy(file_name, gtk_selected_filename);
	load_modelview_file_glut(file_name);
	
	return;
};

