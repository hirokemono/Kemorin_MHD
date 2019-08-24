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
static double gtk_value, gtk_color, gtk_opacity;

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
static void MaxChange(GtkWidget *entry, gpointer data)
{
	gtk_max = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
/*	printf("gtk_max %d\n", gtk_max);*/
}
static void NlineChange(GtkWidget *entry, gpointer data)
{
	gtk_intvalue = (int) gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(entry));
/*	printf("gtk_intvalue %d\n", gtk_min);*/
}

static void AmbientChange(GtkWidget *entry, gpointer data)
{
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemovier_set_material_ambient(value);
/*	printf("gtk_min %d\n", gtk_min);*/
}
static void DiffuseChange(GtkWidget *entry, gpointer data)
{
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_material_diffuse(value);
/*	printf("gtk_min %d\n", gtk_min);*/
}
static void SpecularChange(GtkWidget *entry, gpointer data)
{
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_material_specular(value);
/*	printf("gtk_min %d\n", gtk_min);*/
}
static void ShinenessChange(GtkWidget *entry, gpointer data)
{
	float value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_material_shineness(value);
/*	printf("gtk_min %d\n", gtk_min);*/
}
static void set_opacitymap_gtk(GtkWidget *treeview)
{
	GtkListStore *store;
	GtkTreeIter iter;
	int i;
	double dvalue, dopacity;
	
	/* Get model in treeview and flash all deta */
	store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(treeview)));
	gtk_list_store_clear(store);
	
	
	/* Make new record */
	for(i = 0; i < kemoview_get_PSF_opacity_table_num(); i++) {
		kemoview_get_PSF_opacity_items(i, &dvalue, &dopacity);
		gtk_list_store_append(store, &iter);
		gtk_list_store_set(store, &iter,
					DATA_COL, (float) dvalue,  COLOR_COL, (float) dopacity,
					INDEX_COL,  i, -1); /* End of record */
	};
	return;
}

/* Add one more line in trtwwview */
static void append_column_to_treeview(GtkWidget *treeview,
			const char *title, const int order){
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	
	/* Construct CellRenderer */
	renderer = gtk_cell_renderer_text_new();
	
	//Make columns and add CellRenderer. And connect to model
	column = gtk_tree_view_column_new_with_attributes(
				/* Title for header */
				title,
				/* CEll recndrer */
				renderer,
				/* Connect orders data to "text" */
				"text", order,
				/* end */
				NULL);
	
	/* Add line to treeview */
	gtk_tree_view_append_column(GTK_TREE_VIEW(treeview), column);
	return;
}

static void create_opacity_view(GtkWidget *treeview)
{
  append_column_to_treeview(treeview, "Value", 0);
  append_column_to_treeview(treeview, "Opasity", 1);
/*  append_column_to_treeview(treeview, "Index", 2);*/
	return;
}

/*
   Constract input windows
*/
static void cb_close_window(GtkButton *button, gpointer user_data){
    GtkWidget *window = (GtkWidget *) user_data;
    gtk_widget_destroy(window);
};

static void gtk_colormap_menu(double range_min, double range_max, struct kv_string *title){
    struct colormap_view *color_vws;
	GtkWidget *box;
	
	rangew = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(rangew), title->string);
	
	g_signal_connect(rangew, "destroy", G_CALLBACK(gtk_main_quit), NULL);
	gtk_container_set_border_width(GTK_CONTAINER(rangew), 5);
	
	box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_container_add(GTK_CONTAINER(rangew), box);
	printf("malloc 1\n");
	color_vws = (struct colormap_view *) malloc(sizeof(struct colormap_view));
	printf("init_colormap_views_4_viewer\n");
	init_colormap_views_4_viewer(color_vws);
	
	
	
	printf("add_colormp_list_box\n");
	add_colormp_list_box(color_vws, box);
	printf("button\n");
	GtkButton *button;
	button = gtk_button_new_from_stock(GTK_STOCK_CLOSE);
    gtk_box_pack_start(GTK_BOX(box), button, FALSE, FALSE, 0);
	printf("g_signal_connect\n");
    g_signal_connect(G_OBJECT(button), "clicked", 
                     G_CALLBACK(cb_close_window), rangew);
	printf("gtk_widget_show_all\n");
	
	gtk_widget_show_all(rangew);
	gtk_main();
	printf("dealloc_colormap_views_4_viewer\n");
	dealloc_colormap_views_4_viewer(color_vws);
	printf("free\n");
	free(color_vws);
	return;
}


static void gtk_range_menu(double range_min, double range_max, 
            struct kv_string *title){
	GtkWidget *box;
	GtkWidget *box1, *box2, *box3, *box5;
	GtkWidget *lavel0, *lavel1, *lavel2, *lavel3;
	GtkWidget *bot1, *bot2;
	GtkAdjustment *adj_min, *adj_max;
	
    double delta;
	char min_text[30], max_text[30];
	
	iflag_set = IZERO;
	sprintf(min_text, "    %e    ", range_min);
	sprintf(max_text, "    %e    ", range_max);
	
	rangew = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(rangew), title->string);

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
	lavel1 = gtk_label_new("Maximum");
	gtk_box_pack_start(GTK_BOX(box1), lavel1, TRUE, TRUE, 0);
	
	lavel2 = gtk_label_new(min_text);
	gtk_box_pack_start(GTK_BOX(box2), lavel2, TRUE, TRUE, 0);
	lavel3 = gtk_label_new(max_text);
	gtk_box_pack_start(GTK_BOX(box2), lavel3, TRUE, TRUE, 0);
	
    delta = range_max - range_min;
	adj_min = gtk_adjustment_new (range_min, (range_min-1.0e3*delta), (range_max+1.0e3*delta),
			(delta*1.0e-2), (delta*1.0e-2), 0.0);
	adj_max = gtk_adjustment_new (range_max, (range_min*1.0e3),  (range_max+1.0e3*delta),
			(delta*1.0e-2), (delta*1.0e-2), 0.0);
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

static void set_PSFcolor_GTK(GtkColorChooser *colordialog)
{
	GdkRGBA gcolor;
	gdouble dcolor[4];
	
	gtk_color_chooser_get_rgba(colordialog, &gcolor);
	gtk_widget_destroy(rangew);
	gtk_widget_destroy(ftmpw_w);
	gtk_main_quit();
	
	dcolor[0] = gcolor.red;
	dcolor[1] = gcolor.green;
	dcolor[2] = gcolor.blue;
	dcolor[3] = (gdouble) kemoview_get_PSF_max_opacity();
	kemoview_set_PSF_single_color(dcolor);
	kemoview_set_PSF_patch_color_mode(SINGLE_COLOR);
	draw_mesh_keep_menu();
	return;
}

static void set_background_GTK(GtkColorChooser *colordialog)
{
	GdkRGBA gcolor;
	GLfloat color[4];
	
	gtk_color_chooser_get_rgba(colordialog, &gcolor);
	gtk_widget_destroy(rangew);
	gtk_widget_destroy(ftmpw_w);
	gtk_main_quit();
	
    color[0] = (GLfloat) gcolor.red;
    color[1] = (GLfloat) gcolor.green;
    color[2] = (GLfloat) gcolor.blue;
	/*printf("New background Color (R,G,B): %.7e %.7e %.7e \n", color[0], color[1], color[2]);*/
	
	draw_mesh_keep_menu();
    kemoview_set_background_color(color);
    glClear(GL_COLOR_BUFFER_BIT); 
	
	return;
}

static void kemoview_gtk_PSFcolorsel(GtkButton *button, gpointer data){
	int response;
	GtkColorChooser *chooser;
	GtkWindow *parent;
	
	parent = GTK_WINDOW(g_object_get_data(G_OBJECT(data), "parent"));
	
	rangew = gtk_color_chooser_dialog_new("Choose color", parent);
	gtk_widget_show_all(rangew);
	
	response = gtk_dialog_run(GTK_DIALOG(rangew));
	if (response == GTK_RESPONSE_OK){
		chooser = GTK_COLOR_CHOOSER(rangew);
		set_PSFcolor_GTK(chooser);
		g_print ("color selected \n");
		iflag_set = IONE;
	}
	else if( response == GTK_RESPONSE_CANCEL ){
		g_print( "Cancel button was pressed.\n" );
		gtk_widget_destroy(rangew);
	}
	return;
}

static void gtk_PSFcolorselect(const char *title){
	GtkWidget *hbox;
	GtkWidget *entry;
	GtkWidget *button;
	
	ftmpw_w = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	
	gtk_window_set_title(GTK_WINDOW(ftmpw_w), title);
	gtk_widget_set_size_request(ftmpw_w, 150, -1);
	gtk_container_set_border_width(GTK_CONTAINER(ftmpw_w), 5);
	g_signal_connect(G_OBJECT(ftmpw_w), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	
	hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	
	gtk_container_add(GTK_CONTAINER(ftmpw_w), hbox);
	
	/* Set button   */
	entry = gtk_entry_new();
	button = gtk_button_new_with_label("_select");
	g_signal_connect(G_OBJECT(button), "clicked", 
				G_CALLBACK(kemoview_gtk_PSFcolorsel), (gpointer)entry);
	gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);
	gtk_widget_show_all(ftmpw_w);
	gtk_main();
	return;
}


static void kemoview_gtk_BGcolorsel(GtkButton *button, gpointer data){
	int response;
	GtkColorChooser *chooser;
	GtkWindow *parent;
	
	parent = GTK_WINDOW(g_object_get_data(G_OBJECT(data), "parent"));
	
	rangew = gtk_color_chooser_dialog_new("Choose color", parent);
	gtk_widget_show_all(rangew);
	
	response = gtk_dialog_run(GTK_DIALOG(rangew));
	if (response == GTK_RESPONSE_OK){
		chooser = GTK_COLOR_CHOOSER(rangew);
		set_background_GTK(chooser);
		g_print ("color selected \n");
		iflag_set = IONE;
	}
	else if( response == GTK_RESPONSE_CANCEL ){
		g_print( "Cancel button was pressed.\n" );
		gtk_widget_destroy(rangew);
	}
	return;
}

static void gtk_BGcolorselect(GLfloat color[4], const char *title){
	GtkWidget *label01, *label02, *label03, *label04;
	GtkWidget *label11, *label12, *label13, *label14;
	GtkWidget *label21, *label22, *label23, *label24;
	
	GtkWidget *hbox, *vbox;
	GtkWidget *hbox11, *hbox12, *hbox13, *hbox14;
	GtkWidget *entry;
	GtkWidget *BGselButton, *CloseButton;
	GtkAdjustment *adj1, *adj2, *adj3, *adj4;
	
    struct lightparams_view *lightparams_view;
	char current_text[30];
	float current_value;
	
	ftmpw_w = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	
	gtk_window_set_title(GTK_WINDOW(ftmpw_w), title);
	gtk_widget_set_size_request(ftmpw_w, 150, -1);
	gtk_container_set_border_width(GTK_CONTAINER(ftmpw_w), 5);
	g_signal_connect(G_OBJECT(ftmpw_w), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	
	hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	gtk_container_add(GTK_CONTAINER(ftmpw_w), hbox);
	
	/* Set buttons   */
	entry = gtk_entry_new();
	BGselButton = gtk_button_new_with_label("_select");
	g_signal_connect(G_OBJECT(BGselButton), "clicked", 
				G_CALLBACK(kemoview_gtk_BGcolorsel), (gpointer)entry);
	CloseButton = gtk_button_new_with_label("Close");
	g_signal_connect(G_OBJECT(CloseButton), "clicked", 
				G_CALLBACK(kemoview_gtk_BGcolorsel), (gpointer)entry);
	
	lightparams_view = (struct lightparams_view *) malloc(sizeof(struct lightparams_view));
    init_light_views_4_viewer(lightparams_view);
	
	vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	add_light_list_box(lightparams_view, vbox);
	
	
	label01 = gtk_label_new("Current ambient");
	label02 = gtk_label_new("Current diffuse");
	label03 = gtk_label_new("Current specular");
	label04 = gtk_label_new("Current shineness");
	
	
	current_value = kemoview_send_material_ambient();
	sprintf(current_text, "    %e    ", current_value);
	label21 = gtk_label_new("Ambient:   ");
	adj1 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
	label11 = gtk_label_new(current_text);
	spin1 = gtk_spin_button_new(GTK_ADJUSTMENT(adj1),0,2);
	
	current_value = kemoview_send_material_diffuse();
	sprintf(current_text, "    %e    ", current_value);
	label22 = gtk_label_new("Diffuse:   ");
	label12 = gtk_label_new(current_text);
	adj2 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
	spin2 = gtk_spin_button_new(GTK_ADJUSTMENT(adj2),0,2);
	
	current_value = kemoview_send_material_specular();
	sprintf(current_text, "    %e    ", current_value);
	label23 = gtk_label_new("Specular:  ");
	label13 = gtk_label_new(current_text);
	adj3 = gtk_adjustment_new(current_value, 0.0, 1.0, 0.01, 0.01, 0.0);
	spin3 = gtk_spin_button_new(GTK_ADJUSTMENT(adj3),0,2);
	
	current_value = kemoview_send_material_shiness();
	sprintf(current_text, "    %e    ", current_value);
	label24 = gtk_label_new("Shineness: ");
	label14 = gtk_label_new(current_text);
	adj4 = gtk_adjustment_new(current_value, 0.0, 100.0, 0.1, 0.1, 0.0);
	spin4 = gtk_spin_button_new( GTK_ADJUSTMENT(adj4),0,10.0);
	
	g_signal_connect(spin1, "value-changed", G_CALLBACK(AmbientChange), NULL);
	g_signal_connect(spin2, "value-changed", G_CALLBACK(DiffuseChange), NULL);
	g_signal_connect(spin3, "value-changed", G_CALLBACK(SpecularChange), NULL);
	g_signal_connect(spin4, "value-changed", G_CALLBACK(ShinenessChange), NULL);
	
	hbox11 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	gtk_box_pack_start(GTK_BOX(hbox11), label21, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox11), spin1, FALSE, FALSE, 0);
	
	hbox12 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	gtk_box_pack_start(GTK_BOX(hbox12), label22, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox12), spin2, FALSE, FALSE, 0);
	
	hbox13 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	gtk_box_pack_start(GTK_BOX(hbox13), label23, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox13), spin3, FALSE, FALSE, 0);
	
	hbox14 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
	gtk_box_pack_start(GTK_BOX(hbox14), label24, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox14), spin4, FALSE, FALSE, 0);
	
	
	gtk_box_pack_start(GTK_BOX(vbox), label01, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), label11, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox11, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), label02, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), label12, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox12, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), label03, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), label13, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox13, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), label04, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), label14, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox14, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), CloseButton, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(hbox), BGselButton, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), vbox, FALSE, FALSE, 0);
	
	gtk_widget_show_all(ftmpw_w);
	gtk_main();
	dealloc_colormap_views_4_viewer(lightparams_view);
	free(lightparams_view);
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

void set_psf_single_color_gtk(){
	gtk_PSFcolorselect("Select patch color");
	return;
}

void edit_psf_colormap_gtk(){
	double range_min, range_max;
    struct kv_string *colorname;
	
	int ifield = kemoview_get_PSF_field_id();
	int icomp = kemoview_get_PSF_draw_data_address();
	
    colorname = kemoview_alloc_kvstring();
	kemoview_get_PSF_field_name(colorname, ifield);
	range_min = kemoview_get_PSF_min_data(icomp);
	range_max = kemoview_get_PSF_max_data(icomp);
	
	gtk_colormap_menu(range_min, range_max, colorname);
    kemoview_free_kvstring(colorname);
	/*
	if(iflag_set == MODIFY_POINT){
		kemoview_set_PSF_color_data(i_selected, gtk_value, gtk_color);}
	else if(iflag_set == ADD_POINT) {
		kemoview_add_PSF_color_list(gtk_value, gtk_color);}
	else if(iflag_set == DELETE_POINT) {
		kemoview_delete_PSF_color_list(i_selected);}
	*/
	draw_mesh_keep_menu();
	return;
}

void set_psf_range_gtk(){
	double range_min, range_max;
    struct kv_string *colorname;
	
	int ifield = kemoview_get_PSF_field_id();
	int icomp = kemoview_get_PSF_draw_data_address();
	
    colorname = kemoview_alloc_kvstring();
	kemoview_get_PSF_field_name(colorname, ifield);
	range_min = kemoview_get_PSF_min_data(icomp);
	range_max = kemoview_get_PSF_max_data(icomp);
	
	gtk_range_menu(range_min, range_max, colorname);
    kemoview_free_kvstring(colorname);

    if(iflag_set == IZERO) return; 
	
	kemoview_set_PSF_linear_colormap(gtk_min, gtk_max);
	return;
}

void set_fline_range_gtk(){
	double range_min, range_max;
	
	int ifield = kemoview_get_fline_color_field();
	int icomp = kemoview_get_fline_color_data_adress();
    struct kv_string *colorname = kemoview_alloc_kvstring();

    range_min = kemoview_get_fline_data_min(icomp);
	range_max = kemoview_get_fline_data_max(icomp);
	kemoview_get_fline_color_data_name(colorname, ifield);
	
	gtk_range_menu(range_min, range_max, colorname);
    kemoview_free_kvstring(colorname);

	if(iflag_set == IZERO) return; 
	
	kemoview_set_fline_linear_colormap(gtk_min, gtk_max);
	return;
}

void set_fline_thick_gtk(){
	double thick;
	
	thick = kemoview_get_fline_thickness();
	gtk_opacity_menu(thick, "Set thickness");
	if(iflag_set == IZERO) return; 
	
	if(gtk_min > 0) kemoview_set_fline_thickness(gtk_min);
	return;
}

void set_num_isoline_gtk(){
	int nline;
	
	nline = kemoview_get_PSF_num_isoline();
	gtk_nline_menu(nline, "Set number of lines");
	if(iflag_set == IZERO) return; 
	
	if(nline > 0) kemoview_set_PSF_num_isoline(gtk_intvalue);
	return;
}

void set_psf_vector_increment_gtk(){
	int num_inc;
	
	num_inc = kemoview_get_PSF_vector_increment();
	gtk_nline_menu(num_inc, "Set increment");
	if(iflag_set == IZERO) return; 
	
	if(gtk_intvalue > 0) kemoview_set_PSF_vector_increment(gtk_intvalue);
	return;
}

void set_psf_vector_scale_gtk(){
	double scale_input;
	
	scale_input = kemoview_get_PSF_vector_scale();
	gtk_opacity_menu(scale_input, "Set scale");
	if(iflag_set == IZERO) return; 
	
	scale_input = gtk_min;
	
	if ( scale_input < ZERO) scale_input = ZERO;
	kemoview_set_PSF_vector_scale(scale_input);
	return;
}

void set_psf_vector_thickness_gtk(){
	double thick_input;
	
	thick_input = kemoview_get_PSF_vector_thickness();
	gtk_opacity_menu(thick_input, "Set thickness");
	if(iflag_set == IZERO) return; 
	
	thick_input = gtk_min;
	
	if (thick_input < ZERO) thick_input = ZERO;
	kemoview_set_PSF_vector_thickness(thick_input);
	return;
}

void set_psf_opacity_gtk(){
	double opacity;
	
	opacity = kemoview_get_PSF_max_opacity();
	gtk_opacity_menu(opacity, "Set opacity");
	if(iflag_set == IZERO) return; 
	
	kemoview_set_PSF_constant_opacity(gtk_min);
	return;
}

void set_domain_opacity_gtk(){
	double opacity;
	
	opacity = kemoview_get_domain_opacity();
	gtk_opacity_menu(opacity, "Set opacity");
	if(iflag_set == IZERO) return; 
	
	kemoview_set_domain_opacity(gtk_min);
	return;
}

void set_ele_group_opacity_gtk(){
	double opacity;
	
	opacity = kemoview_get_surf_grp_opacity();
	gtk_opacity_menu(opacity, "Set opacity");
	if(iflag_set == IZERO) return; 
	
	kemoview_set_ele_grp_opacity(gtk_min);
	return;
}

void set_surf_group_opacity_gtk(){
	double opacity;
	
	opacity = kemoview_get_surf_grp_opacity();
	gtk_opacity_menu(opacity, "Set opacity");
	if(iflag_set == IZERO) return; 
	
	kemoview_set_surf_grp_opacity(gtk_min);
	return;
}

void set_coastline_radius_gtk(){
	double radius;
	
	radius = kemoview_get_coastline_radius();
	gtk_opacity_menu(radius, "Set radius");
	if(iflag_set == IZERO) return; 
	
	kemoview_set_coastline_radius(gtk_min);
	return;
};

void set_background_color_gtk(){
    GLfloat color[4];
	
	kemoview_get_background_color(color);
	gtk_BGcolorselect(color, "Select Background color");
    return;
};

void set_domain_distance_gtk(){
	double distance;
	
	distance = kemoview_get_domain_distance();
	gtk_opacity_menu(distance, "Set distance");
	if(iflag_set == IZERO) return; 
	
	kemoview_set_domain_distance(gtk_min);
	return;
}

void set_num_color_loop_gtk(){
	int num_cloop;
	
	num_cloop = kemoview_get_num_of_color_loop();
	gtk_nline_menu(num_cloop, "Set loop number");
	if(iflag_set == IZERO) return; 
	
	kemoview_set_num_of_color_loop(gtk_intvalue);
	return;
}

void set_node_size_gtk(){
	double nodesize;
	
	nodesize = kemoview_get_node_diamater();
	gtk_opacity_menu(nodesize, "Set size");
	if(iflag_set == IZERO) return; 
	
	kemoview_set_node_diamater(gtk_min);
	return;
}

