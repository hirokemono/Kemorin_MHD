/*
 *  kemoview_gtk_window_input.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_window_input.h"

GtkWidget *rangew;
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
	gtk_min = (double) gtk_spin_button_get_value_as_float(GTK_SPIN_BUTTON(entry));
/*	printf("gtk_min %d\n", gtk_min);*/
}
static void MaxChange(GtkWidget *entry, gpointer data)
{
	gtk_max = (double) gtk_spin_button_get_value_as_float(GTK_SPIN_BUTTON(entry));
/*	printf("gtk_max %d\n", gtk_max);*/
}
static void NlineChange(GtkWidget *entry, gpointer data)
{
	gtk_intvalue = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
/*	printf("gtk_intvalue %d\n", gtk_min);*/
}


/*
   Constract input windows
*/

static void gtk_range_menu(double range_min, double range_max, 
			const char *title){
	GtkWidget *box;
	GtkWidget *box1, *box2, *box3, *box5;
	GtkWidget *spin1, *spin2;
	GtkWidget *lavel0, *lavel1, *lavel2, *lavel3;
	GtkWidget *bot1, *bot2;
	GtkObject *adj_min, *adj_max;
	
	char min_text[30], max_text[30];
	
	iflag_set = IZERO;
	sprintf(min_text, "    %e    ", range_min);
	sprintf(max_text, "    %e    ", range_max);
	
	rangew = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(rangew), title);

	g_signal_connect(rangew, "destroy", G_CALLBACK(gtk_main_quit), NULL);

	gtk_container_set_border_width(GTK_CONTAINER(rangew), 5);

	
	box = gtk_vbox_new(FALSE, 10);
	gtk_container_add(GTK_CONTAINER(rangew), box);
	
	box1 = gtk_hbox_new(FALSE, 5);
	gtk_container_add(GTK_CONTAINER(box), box1);
	box2 = gtk_hbox_new(FALSE, 5);
	gtk_container_add(GTK_CONTAINER(box), box2);
	box3 = gtk_hbox_new(FALSE, 5);
	gtk_container_add(GTK_CONTAINER(box), box3);
	box5 = gtk_hbox_new(FALSE, 5);
	gtk_container_add(GTK_CONTAINER(box), box5);
	
	
	lavel0 = gtk_label_new("Minimum");
	gtk_box_pack_start(GTK_BOX(box1), lavel0, TRUE, TRUE, 0);
	lavel1 = gtk_label_new("Maximum");
	gtk_box_pack_start(GTK_BOX(box1), lavel1, TRUE, TRUE, 0);
	
	lavel2 = gtk_label_new(min_text);
	gtk_box_pack_start(GTK_BOX(box2), lavel2, TRUE, TRUE, 0);
	lavel3 = gtk_label_new(max_text);
	gtk_box_pack_start(GTK_BOX(box2), lavel3, TRUE, TRUE, 0);
	
	adj_min = gtk_adjustment_new (range_min, (range_min*1.0e3), (range_max*1.0e3),
			((range_max-range_min)*1.0e-2), ((range_max-range_min)*1.0e-2), 0.0);
	adj_max = gtk_adjustment_new (range_max, (range_min*1.0e3), (range_max*1.0e3),
			((range_max-range_min)*1.0e-2), ((range_max-range_min)*1.0e-2), 0.0);
	spin1 = gtk_spin_button_new( GTK_ADJUSTMENT(adj_min),0,2);
	gtk_box_pack_start(GTK_BOX(box3), spin1, TRUE, TRUE, 0);
	spin2 = gtk_spin_button_new( GTK_ADJUSTMENT(adj_max),0,2);
	gtk_box_pack_start(GTK_BOX(box3), spin2, TRUE, TRUE, 0);
	
	bot1 = gtk_button_new_with_label("Cancel");
	gtk_box_pack_start(GTK_BOX(box5), bot1, FALSE, FALSE, 0);
	bot2 = gtk_button_new_with_label("Save");
	gtk_box_pack_start(GTK_BOX(box5), bot2, FALSE, FALSE, 0);

	g_signal_connect(spin1, "value-changed", G_CALLBACK(MinChange), NULL);
	g_signal_connect(spin2, "value-changed", G_CALLBACK(MaxChange), NULL);
	g_signal_connect(bot1, "clicked", G_CALLBACK(destroy), NULL);
	g_signal_connect(bot2, "clicked", G_CALLBACK(OK_clicked), NULL);

	gtk_widget_show_all(rangew);

	gtk_main();

	return;
}

static void gtk_opacity_menu(double current_value, const char *title){
	GtkWidget *box;
	GtkWidget *box1, *box2, *box3, *box5;
	GtkWidget *spin1;
	GtkWidget *lavel0, *lavel2;
	GtkWidget *bot2;
	GtkObject *adj;
	
	char current_text[30];
	
	iflag_set = IZERO;
	sprintf(current_text, "    %e    ", current_value);
	
	rangew = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(rangew), title);

	g_signal_connect(rangew, "destroy", G_CALLBACK(gtk_main_quit), NULL);

	gtk_container_set_border_width(GTK_CONTAINER(rangew), 5);

	
	box = gtk_vbox_new(FALSE, 10);
	gtk_container_add(GTK_CONTAINER(rangew), box);
	
	box1 = gtk_hbox_new(FALSE, 5);
	gtk_container_add(GTK_CONTAINER(box), box1);
	box2 = gtk_hbox_new(FALSE, 5);
	gtk_container_add(GTK_CONTAINER(box), box2);
	box3 = gtk_hbox_new(FALSE, 5);
	gtk_container_add(GTK_CONTAINER(box), box3);
	box5 = gtk_hbox_new(FALSE, 5);
	gtk_container_add(GTK_CONTAINER(box), box5);
	
	
	lavel0 = gtk_label_new("Current value");
	gtk_box_pack_start(GTK_BOX(box1), lavel0, TRUE, TRUE, 0);
	
	lavel2 = gtk_label_new(current_text);
	gtk_box_pack_start(GTK_BOX(box2), lavel2, TRUE, TRUE, 0);
	
	adj = gtk_adjustment_new (current_value, ZERO, ONE, 0.01, 0.01, 0.0);
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
	GtkWidget *spin1;
	GtkWidget *lavel0, *lavel2;
	GtkWidget *bot2;
	GtkObject *adj;
	
	char min_text[30];
	
	iflag_set = IZERO;
	sprintf(min_text, "       %d        ", nline);
	
	rangew = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(rangew), title);

	g_signal_connect(rangew, "destroy", G_CALLBACK(gtk_main_quit), NULL);

	gtk_container_set_border_width(GTK_CONTAINER(rangew), 5);

	
	box = gtk_vbox_new(FALSE, 10);
	gtk_container_add(GTK_CONTAINER(rangew), box);
	
	box1 = gtk_hbox_new(FALSE, 5);
	gtk_container_add(GTK_CONTAINER(box), box1);
	box2 = gtk_hbox_new(FALSE, 5);
	gtk_container_add(GTK_CONTAINER(box), box2);
	box3 = gtk_hbox_new(FALSE, 5);
	gtk_container_add(GTK_CONTAINER(box), box3);
	box5 = gtk_hbox_new(FALSE, 5);
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

void set_psf_range_gtk(){
	double range_min, range_max;
	char name[1024];
	
	int ifield = send_draw_field_current_psf();
	int icomp = send_draw_component_current_psf();
	
	send_current_psf_data_name(name, ifield);
	range_min = send_current_psf_data_min(icomp);
	range_max = send_current_psf_data_max(icomp);
	
	gtk_range_menu(range_min, range_max, name);
	if(iflag_set == IZERO) return; 
	
	set_current_PSF_linear_colormap(gtk_min, gtk_max);
	return;
}

void set_fline_range_gtk(){
	double range_min, range_max;
	char name[1024];
	
	int ifield = send_if_draw_fline();
	int icomp = send_icomp_draw_fline();
	range_min = send_fline_data_min(icomp);
	range_max = send_fline_data_max(icomp);
	send_fline_data_name(name, ifield);
	
	gtk_range_menu(range_min, range_max, name);
	if(iflag_set == IZERO) return; 
	
	input_fline_linear_colormap(gtk_min, gtk_max);
	return;
}

void set_fline_thick_gtk(){
	double thick;
	
	thick = send_fline_thickness();
	gtk_opacity_menu(thick, "Set thickness");
	if(iflag_set == IZERO) return; 
	
	if(gtk_min > 0) set_to_fline_thickness(gtk_min);
	return;
}

void set_num_isoline_gtk(){
	int nline;
	
	nline = send_current_num_isoline();
	gtk_nline_menu(nline, "Set number of lines");
	if(iflag_set == IZERO) return; 
	
	if(nline > 0) set_current_n_isoline(gtk_intvalue);
	return;
}

void set_psf_vector_increment_gtk(){
	int num_inc;
	
	num_inc = send_current_increment_vect();
	gtk_nline_menu(num_inc, "Set increment");
	if(iflag_set == IZERO) return; 
	
	if(gtk_intvalue > 0) set_current_increment_vect(gtk_intvalue);
	return;
}

void set_psf_vector_scale_gtk(){
	double scale_input;
	
	scale_input = send_current_scale_vect();
	gtk_opacity_menu(scale_input, "Set scale");
	if(iflag_set == IZERO) return; 
	
	scale_input = gtk_min;
	
	if ( scale_input < ZERO) scale_input = ZERO;
	set_current_scale_vect(scale_input);
	return;
}

void set_psf_vector_thickness_gtk(){
	double thick_input;
	
	thick_input = send_current_vector_thick();
	gtk_opacity_menu(thick_input, "Set thickness");
	if(iflag_set == IZERO) return; 
	
	thick_input = gtk_min;
	
	if (thick_input < ZERO) thick_input = ZERO;
	set_current_vector_thick(thick_input);
	return;
}

void set_psf_opacity_gtk(){
	double opacity;
	
	opacity = send_current_PSF_maximum_opacity();
	gtk_opacity_menu(opacity, "Set opacity");
	if(iflag_set == IZERO) return; 
	
	set_current_PSF_constant_opacity(gtk_min);
	return;
}

void set_domain_opacity_gtk(){
	double opacity;
	
	opacity = send_domain_surface_opacity();
	gtk_opacity_menu(opacity, "Set opacity");
	if(iflag_set == IZERO) return; 
	
	set_to_domain_surface_opacity(gtk_min);
	return;
}

void set_ele_group_opacity_gtk(){
	double opacity;
	
	opacity = send_surf_surface_opacity();
	gtk_opacity_menu(opacity, "Set opacity");
	if(iflag_set == IZERO) return; 
	
	set_to_ele_surface_opacity(gtk_min);
	return;
}

void set_surf_group_opacity_gtk(){
	double opacity;
	
	opacity = send_surf_surface_opacity();
	gtk_opacity_menu(opacity, "Set opacity");
	if(iflag_set == IZERO) return; 
	
	set_to_surf_surface_opacity(gtk_min);
	return;
}

void set_coastline_radius_gtk(){
	double radius;
	
	radius = send_coastline_radius();
	gtk_opacity_menu(radius, "Set radius");
	if(iflag_set == IZERO) return; 
	
	set_to_coastline_radius(gtk_min);
	return;
};

void set_domain_distance_gtk(){
	double distance;
	
	distance = send_dist_domains();
	gtk_opacity_menu(distance, "Set distance");
	if(iflag_set == IZERO) return; 
	
	set_to_dist_domains(gtk_min);
	return;
}

void set_num_color_loop_gtk(){
	int num_cloop;
	
	num_cloop = send_num_of_color_loop();
	gtk_nline_menu(num_cloop, "Set loop number");
	if(iflag_set == IZERO) return; 
	
	set_to_num_of_color_loop(gtk_intvalue);
	return;
}

void set_node_size_gtk(){
	double nodesize;
	
	nodesize = send_node_diam();
	gtk_opacity_menu(nodesize, "Set size");
	if(iflag_set == IZERO) return; 
	
	set_to_node_diam(gtk_min);
	return;
}

