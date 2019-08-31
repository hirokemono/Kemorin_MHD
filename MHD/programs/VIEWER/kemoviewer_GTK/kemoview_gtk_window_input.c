/*
 *  kemoview_gtk_window_input.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_window_input.h"

#define DATA_COL 0
#define COLOR_COL 1
#define INDEX_COL 2

#define MODIFY_POINT 2
#define ADD_POINT    3
#define DELETE_POINT 4

GtkWidget *rangew;
GtkWidget *ftmpw_w;
GtkWidget *spin1, *spin2, *spin3, *spin4;

static int i_selected;
static int iflag_set;
static int gtk_intvalue;
static double gtk_min, gtk_max;

/*
GTK callback routines
*/
static void destroy (GtkWidget *widget, gpointer data)
{
	gtk_widget_destroy(rangew);
	gtk_main_quit();
};

static void OK_clicked(GtkWidget *widget, gpointer data)
{
	iflag_set = IONE;
	gtk_widget_destroy(rangew);
	gtk_main_quit();
}


static void MinChange(GtkWidget *entry, gpointer data)
{
	gtk_min = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
/*	printf("gtk_min %d\n", gtk_min);*/
}

static void NlineChange(GtkWidget *entry, gpointer data)
{
	gtk_intvalue = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
/*	printf("gtk_intvalue %d\n", gtk_min);*/
}

/*
   Constract input windows
*/
static void gtk_opacity_menu(double current_value, const char *title){
	GtkWidget *box;
	GtkWidget *box1, *box2, *box3, *box5;
	GtkWidget *lavel0, *lavel2;
	GtkWidget *bot2;
	GtkAdjustment *adj;
	
	char current_text[30];
	
	iflag_set = IZERO;
	
	rangew = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(rangew), title);

	g_signal_connect(rangew, "destroy", G_CALLBACK(gtk_main_quit), NULL);

	gtk_container_set_border_width(GTK_CONTAINER(rangew), 5);

	
	box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_container_add(GTK_CONTAINER(rangew), box);
	
	box1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), box1);
	box2 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), box2);
	box3 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), box3);
	box5 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), box5);
	
	
	lavel0 = gtk_label_new("Current value");
	gtk_box_pack_start(GTK_BOX(box1), lavel0, TRUE, TRUE, 0);
	
	sprintf(current_text, "    %e    ", current_value);
	lavel2 = gtk_label_new(current_text);
	gtk_box_pack_start(GTK_BOX(box2), lavel2, TRUE, TRUE, 0);
	
	adj = gtk_adjustment_new(current_value, ZERO, ONE, 0.01, 0.01, 0.0);
	spin1 = gtk_spin_button_new( GTK_ADJUSTMENT(adj),0,2);
	gtk_box_pack_start(GTK_BOX(box3), spin1, TRUE, TRUE, 0);
	
	bot2 = gtk_button_new_with_label("Set");
	gtk_box_pack_start(GTK_BOX(box5), bot2, FALSE, FALSE, 0);

	g_signal_connect(spin1, "value-changed", G_CALLBACK(MinChange), NULL);
	g_signal_connect(bot2, "clicked", G_CALLBACK(OK_clicked), NULL);

	gtk_widget_show_all(rangew);

	gtk_main();

	return;
}

static void gtk_nline_menu(int nline, const char *title){
	GtkWidget *box;
	GtkWidget *box1, *box2, *box3, *box5;
	GtkWidget *lavel0, *lavel2;
	GtkWidget *bot2;
	GtkAdjustment *adj;
	
	char min_text[30];
	
	iflag_set = IZERO;
	sprintf(min_text, "       %d        ", nline);
	
	rangew = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(rangew), title);

	g_signal_connect(rangew, "destroy", G_CALLBACK(gtk_main_quit), NULL);

	gtk_container_set_border_width(GTK_CONTAINER(rangew), 5);

	
	box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_container_add(GTK_CONTAINER(rangew), box);
	
	box1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), box1);
	box2 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), box2);
	box3 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), box3);
	box5 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), box5);
	
	
	lavel0 = gtk_label_new("Minimum");
	gtk_box_pack_start(GTK_BOX(box1), lavel0, TRUE, TRUE, 0);
	
	lavel2 = gtk_label_new(min_text);
	gtk_box_pack_start(GTK_BOX(box2), lavel2, TRUE, TRUE, 0);
	
	adj = gtk_adjustment_new ((double) nline, ZERO, 1000, 1, 1, 0.0);
	spin1 = gtk_spin_button_new( GTK_ADJUSTMENT(adj),0,2);
	gtk_box_pack_start(GTK_BOX(box3), spin1, TRUE, TRUE, 0);
	
	bot2 = gtk_button_new_with_label("Set");
	gtk_box_pack_start(GTK_BOX(box5), bot2, FALSE, FALSE, 0);

	g_signal_connect(spin1, "value-changed", G_CALLBACK(NlineChange), NULL);
	g_signal_connect(bot2, "clicked", G_CALLBACK(OK_clicked), NULL);

	gtk_widget_show_all(rangew);

	gtk_main();

	return;
}

/* Routines for values from console input */

void set_coastline_radius_gtk(){
	double radius;
	
	radius = kemoview_get_coastline_radius();
	gtk_opacity_menu(radius, "Set radius");
	if(iflag_set == IZERO) return; 
	
	kemoview_set_coastline_radius(gtk_min);
	return;
};

void set_num_color_loop_gtk(){
	int num_cloop;
	
	num_cloop = kemoview_get_num_of_color_loop();
	gtk_nline_menu(num_cloop, "Set loop number");
	if(iflag_set == IZERO) return; 
	
	kemoview_set_num_of_color_loop(gtk_intvalue);
	return;
}

