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
GtkWidget *ftmpw;
GtkWidget *spin1, *spin2;

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

static void OK_update(GtkWidget *widget, gpointer data)
{
	iflag_set = MODIFY_POINT;
	gtk_widget_destroy(rangew);
	gtk_main_quit();
}

static void OK_add(GtkWidget *widget, gpointer data)
{
	iflag_set = ADD_POINT;
	gtk_widget_destroy(rangew);
	gtk_main_quit();
}

static void OK_delete(GtkWidget *widget, gpointer data)
{
	iflag_set = DELETE_POINT;
	gtk_widget_destroy(rangew);
	gtk_main_quit();
}

static void cb_tree_clicked (GtkTreeSelection *selection, gpointer user_data)
{
	GtkTreeIter iter;
	GtkTreeModel *model;
	gint sel;
	gfloat value, color;
	
	if (gtk_tree_selection_get_selected (selection, &model, &iter)){
		gtk_tree_model_get (model, &iter, DATA_COL, &value,  -1);
		gtk_tree_model_get (model, &iter, COLOR_COL, &color, -1);
		gtk_tree_model_get (model, &iter, INDEX_COL, &sel, -1);
/*		g_print ("You selected index %d\n", sel); */
		i_selected = (int) sel;
		gtk_spin_button_set_value (GTK_SPIN_BUTTON(spin1), (gdouble) value);
		gtk_spin_button_set_value (GTK_SPIN_BUTTON(spin2), (gdouble) color);
	}
}

static void dataChange(GtkWidget *entry, gpointer data)
{
	gtk_value = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
}
static void colorChange(GtkWidget *entry, gpointer data)
{
	gtk_color = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
}
static void opacityChange(GtkWidget *entry, gpointer data)
{
	gtk_opacity = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
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

/*
Construct color table
*/
static void create_colormap_model(GtkWidget *treeview){
  GtkListStore *store;

/* Construct GtkListStore */
	store = gtk_list_store_new(
	3,  /* Number of Component */
	G_TYPE_FLOAT, /* Data */
	G_TYPE_FLOAT, /* Color */
	G_TYPE_INT /* index */
  );

  /* Link to treeview */
  gtk_tree_view_set_model(GTK_TREE_VIEW(treeview), GTK_TREE_MODEL(store));

  /* Store can release after link to treeview */
  g_object_unref(store);
	return;
}

static void set_colormap_gtk(GtkWidget *treeview)
{
	GtkListStore *store;
	GtkTreeIter iter;
	int i;
	double dvalue, dcolor;
	
	/* Get model in treeview and flash all deta */
	store = GTK_LIST_STORE(gtk_tree_view_get_model(GTK_TREE_VIEW(treeview)));
	gtk_list_store_clear(store);
	
	
	/* Make new record */
	for(i = 0; i < kemoview_get_PSF_color_table_num(); i++) {
		kemoview_get_PSF_color_items(i, &dvalue, &dcolor);
		gtk_list_store_append(store, &iter);
		gtk_list_store_set(store, &iter,
					DATA_COL, (float) dvalue, /* Data */
					COLOR_COL, (float) dcolor, /* Color */
					INDEX_COL,  i,  /* Index */
					-1); /* End of record */
	};
	return;
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

/* Defeine treeview to view */
static void create_colormap_view(GtkWidget *treeview)
{
  append_column_to_treeview(treeview, "Value", 0);
  append_column_to_treeview(treeview, "Color", 1);
/*  append_column_to_treeview(treeview, "Index", 2);*/
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

static void gtk_colormap_menu(double range_min, double range_max, const char *title){
	GtkWidget *treeview;
	GtkTreeSelection *selection;
	GtkWidget *box;
	GtkWidget *hbox1, *hbox2, *hbox3, *hbox4, *hbox5, *hbox6;
	GtkWidget *vbox1;
	GtkWidget *lavel0, *lavel1, *lavel2, *lavel3, *lavel4;
	GtkWidget *bot1, *bot2, *bot3, *bot4;
	GtkObject *adj_data, *adj_color;
	
	char min_text[30], max_text[30];
	
	
	/* Generate treeview */
	treeview = gtk_tree_view_new();
	/* model definition */
	create_colormap_model(treeview);
	/* Add data into model */
	set_colormap_gtk(treeview);
	/* Define view */
	create_colormap_view(treeview);
	
	selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(treeview));
	gtk_tree_selection_set_mode(selection, GTK_SELECTION_SINGLE);
	g_signal_connect(G_OBJECT(selection), "changed", G_CALLBACK(cb_tree_clicked), NULL);
	
	iflag_set = IZERO;
	sprintf(min_text, "    %e    ", range_min);
	sprintf(max_text, "    %e    ", range_max);
	
	
	rangew = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(rangew), title);
	
	g_signal_connect(rangew, "destroy", G_CALLBACK(gtk_main_quit), NULL);
	
	gtk_container_set_border_width(GTK_CONTAINER(rangew), 5);
	
	
	box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_container_add(GTK_CONTAINER(rangew), box);
	
	hbox1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), hbox1);
	hbox2 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), hbox2);
	vbox1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_container_add(GTK_CONTAINER(box), vbox1);
	hbox4 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), hbox4);
	hbox3 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), hbox3);
	hbox5 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), hbox5);
	hbox6 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), hbox6);
	
	
	lavel0 = gtk_label_new("Minimum");
	gtk_box_pack_start(GTK_BOX(hbox1), lavel0, TRUE, TRUE, 0);
	lavel1 = gtk_label_new("Maximum");
	gtk_box_pack_start(GTK_BOX(hbox1), lavel1, TRUE, TRUE, 0);
	
	lavel2 = gtk_label_new(min_text);
	gtk_box_pack_start(GTK_BOX(hbox2), lavel2, TRUE, TRUE, 0);
	lavel3 = gtk_label_new(max_text);
	gtk_box_pack_start(GTK_BOX(hbox2), lavel3, TRUE, TRUE, 0);
	
	/* Create a radio button with a GtkEntry widget */
	
	gtk_box_pack_start(GTK_BOX(vbox1), treeview, TRUE, TRUE, 0);
	
	kemoview_get_PSF_color_items(0, &gtk_value, &gtk_color);
	adj_data = gtk_adjustment_new(gtk_value, (range_min*1.0e3), (range_max*1.0e3),
			((range_max-range_min)*1.0e-2), ((range_max-range_min)*1.0e-2), 0.0);
	adj_color = gtk_adjustment_new (gtk_color, 0.0, 1.0, 0.01, 0.01, 0.0);
	
	
	lavel4 = gtk_label_new("Data	|	Color");
	gtk_box_pack_start(GTK_BOX(hbox4), lavel4, TRUE, TRUE, 0);
	spin1 = gtk_spin_button_new( GTK_ADJUSTMENT(adj_data),0,2);
	gtk_box_pack_start(GTK_BOX(hbox3), spin1, TRUE, TRUE, 0);
	spin2 = gtk_spin_button_new( GTK_ADJUSTMENT(adj_color),0,2);
	gtk_box_pack_start(GTK_BOX(hbox3), spin2, TRUE, TRUE, 0);
	
	bot1 = gtk_button_new_with_label("Cancel");
	gtk_box_pack_start(GTK_BOX(hbox5), bot1, FALSE, FALSE, 0);
	bot2 = gtk_button_new_with_label("Update selected");
	gtk_box_pack_start(GTK_BOX(hbox5), bot2, FALSE, FALSE, 0);
	bot3 = gtk_button_new_with_label("Add point");
	gtk_box_pack_start(GTK_BOX(hbox6), bot3, FALSE, FALSE, 0);
	bot4 = gtk_button_new_with_label("Delete selected");
	gtk_box_pack_start(GTK_BOX(hbox6), bot4, FALSE, FALSE, 0);

	g_signal_connect(spin1, "value-changed", G_CALLBACK(dataChange), NULL);
	g_signal_connect(spin2, "value-changed", G_CALLBACK(colorChange), NULL);
	g_signal_connect(bot1, "clicked", G_CALLBACK(destroy), NULL);
	g_signal_connect(bot2, "clicked", G_CALLBACK(OK_update), NULL);
	g_signal_connect(bot3, "clicked", G_CALLBACK(OK_add), NULL);
	g_signal_connect(bot4, "clicked", G_CALLBACK(OK_delete), NULL);
	
	gtk_widget_show_all(rangew);
	
	gtk_main();
	return;
}


static void gtk_opacitymap_menu(double range_min, double range_max, const char *title){
	GtkWidget *treeview;
	GtkTreeSelection *selection;
	GtkWidget *box;
	GtkWidget *hbox1, *hbox2, *hbox3, *hbox4, *hbox5, *hbox6;
	GtkWidget *vbox1;
	GtkWidget *lavel0, *lavel1, *lavel2, *lavel3, *lavel4;
	GtkWidget *bot1, *bot2, *bot3, *bot4;
	GtkObject *adj_data, *adj_opasity;
	
	char min_text[30], max_text[30];
	
	
	/* Generate treeview */
	treeview = gtk_tree_view_new();
	/* model definition */
	create_colormap_model(treeview);
	/* Add data into model */
	set_opacitymap_gtk(treeview);
	/* Define view */
	create_opacity_view(treeview);
	
	selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(treeview));
	gtk_tree_selection_set_mode(selection, GTK_SELECTION_SINGLE);
	g_signal_connect(G_OBJECT(selection), "changed", G_CALLBACK(cb_tree_clicked), NULL);

	
	iflag_set = IZERO;
	sprintf(min_text, "    %e    ", range_min);
	sprintf(max_text, "    %e    ", range_max);
	
	
	rangew = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(rangew), title);
	
	g_signal_connect(rangew, "destroy", G_CALLBACK(gtk_main_quit), NULL);
	
	gtk_container_set_border_width(GTK_CONTAINER(rangew), 5);
	
	
	box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
	gtk_container_add(GTK_CONTAINER(rangew), box);
	
	hbox1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), hbox1);
	hbox2 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), hbox2);
	vbox1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_container_add(GTK_CONTAINER(box), vbox1);
	hbox4 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), hbox4);
	hbox3 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), hbox3);
	hbox5 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), hbox5);
	hbox6 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_container_add(GTK_CONTAINER(box), hbox6);
	
	
	lavel0 = gtk_label_new("Minimum");
	gtk_box_pack_start(GTK_BOX(hbox1), lavel0, TRUE, TRUE, 0);
	lavel1 = gtk_label_new("Maximum");
	gtk_box_pack_start(GTK_BOX(hbox1), lavel1, TRUE, TRUE, 0);
	
	lavel2 = gtk_label_new(min_text);
	gtk_box_pack_start(GTK_BOX(hbox2), lavel2, TRUE, TRUE, 0);
	lavel3 = gtk_label_new(max_text);
	gtk_box_pack_start(GTK_BOX(hbox2), lavel3, TRUE, TRUE, 0);
	
	/* Create a radio button with a GtkEntry widget */
	
	gtk_box_pack_start(GTK_BOX(vbox1), treeview, TRUE, TRUE, 0);
	
	kemoview_get_PSF_opacity_items(0, &gtk_value, &gtk_opacity);
	adj_data = gtk_adjustment_new(gtk_value, (range_min*1.0e3), (range_max*1.0e3),
			((range_max-range_min)*1.0e-2), ((range_max-range_min)*1.0e-2), 0.0);
	adj_opasity = gtk_adjustment_new (gtk_opacity, 0.0, 1.0, 0.01, 0.01, 0.0);
	
	
	lavel4 = gtk_label_new("Data	|	Opasity");
	gtk_box_pack_start(GTK_BOX(hbox4), lavel4, TRUE, TRUE, 0);
	spin1 = gtk_spin_button_new( GTK_ADJUSTMENT(adj_data),0,2);
	gtk_box_pack_start(GTK_BOX(hbox3), spin1, TRUE, TRUE, 0);
	spin2 = gtk_spin_button_new( GTK_ADJUSTMENT(adj_opasity),0,2);
	gtk_box_pack_start(GTK_BOX(hbox3), spin2, TRUE, TRUE, 0);
	
	bot1 = gtk_button_new_with_label("Cancel");
	gtk_box_pack_start(GTK_BOX(hbox5), bot1, FALSE, FALSE, 0);
	bot2 = gtk_button_new_with_label("Update selected");
	gtk_box_pack_start(GTK_BOX(hbox5), bot2, FALSE, FALSE, 0);
	bot3 = gtk_button_new_with_label("Add point");
	gtk_box_pack_start(GTK_BOX(hbox6), bot3, FALSE, FALSE, 0);
	bot4 = gtk_button_new_with_label("Delete selected");
	gtk_box_pack_start(GTK_BOX(hbox6), bot4, FALSE, FALSE, 0);

	g_signal_connect(spin1, "value-changed", G_CALLBACK(dataChange), NULL);
	g_signal_connect(spin2, "value-changed", G_CALLBACK(opacityChange), NULL);
	g_signal_connect(bot1, "clicked", G_CALLBACK(destroy), NULL);
	g_signal_connect(bot2, "clicked", G_CALLBACK(OK_update), NULL);
	g_signal_connect(bot3, "clicked", G_CALLBACK(OK_add), NULL);
	g_signal_connect(bot4, "clicked", G_CALLBACK(OK_delete), NULL);
	
	gtk_widget_show_all(rangew);
	
	gtk_main();
	return;
}


static void gtk_range_menu(double range_min, double range_max, 
			const char *title){
	GtkWidget *box;
	GtkWidget *box1, *box2, *box3, *box5;
	GtkWidget *lavel0, *lavel1, *lavel2, *lavel3;
	GtkWidget *bot1, *bot2;
	GtkObject *adj_min, *adj_max;
	
    double delta;
	char min_text[30], max_text[30];
	
	iflag_set = IZERO;
	sprintf(min_text, "    %e    ", range_min);
	sprintf(max_text, "    %e    ", range_max);
	
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
	GtkObject *adj;
	
	char current_text[30];
	
	iflag_set = IZERO;
	sprintf(current_text, "    %e    ", current_value);
	
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

static void set_PSFcolor_GTK(GtkWidget *colordialog)
{
	GtkWidget* csel;
	gdouble dcolor[4];
	
	csel = gtk_color_selection_dialog_get_color_selection (GTK_COLOR_SELECTION_DIALOG(colordialog));
	gtk_color_selection_get_color( GTK_COLOR_SELECTION(csel), dcolor);
	gtk_widget_destroy(colordialog);
	gtk_widget_destroy(ftmpw);
	gtk_main_quit();
	
	dcolor[3] = (float) kemoview_get_PSF_max_opacity();
	kemoview_set_PSF_single_color(dcolor);
	kemoview_set_PSF_patch_color_mode(SINGLE_COLOR);
	draw_mesh_keep_menu();
	return;
}

static void set_background_GTK(GtkWidget *colordialog)
{
	GtkWidget* csel;
	GLfloat color[4];
	gdouble dcolor[4];
	
	csel = gtk_color_selection_dialog_get_color_selection (GTK_COLOR_SELECTION_DIALOG(colordialog));
	gtk_color_selection_get_color( GTK_COLOR_SELECTION(csel), dcolor);
	gtk_widget_destroy(colordialog);
	gtk_widget_destroy(ftmpw);
	gtk_main_quit();
	
    color[0] = (GLfloat) dcolor[0];
    color[1] = (GLfloat) dcolor[1];
    color[2] = (GLfloat) dcolor[2];
	/*printf("New background Color (R,G,B): %.7e %.7e %.7e \n", color[0], color[1], color[2]);*/
	
	draw_mesh_keep_menu();
    kemoview_set_background_color(color);
    glClear(GL_COLOR_BUFFER_BIT); 
	
	return;
}

static void kemoview_gtk_PSFcolorsel(GtkButton *button, gpointer data){
	int response;
	GtkWidget *parent;
	GtkEntry *entry;
	parent = GTK_WIDGET(g_object_get_data(G_OBJECT(data), "parent"));
	entry = GTK_ENTRY(data);
	
	rangew = gtk_color_selection_dialog_new("Choose color");
	gtk_widget_show_all(rangew);
	
	response = gtk_dialog_run(GTK_DIALOG(rangew));
	if (response == GTK_RESPONSE_OK){
		set_PSFcolor_GTK(rangew);
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
	
	ftmpw = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	
	gtk_window_set_title(GTK_WINDOW(ftmpw), title);
	gtk_widget_set_size_request(ftmpw, 150, -1);
	gtk_container_set_border_width(GTK_CONTAINER(ftmpw), 5);
	g_signal_connect(G_OBJECT(ftmpw), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	
	/*hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);*/
	hbox = gtk_hbox_new(FALSE, 0);
	
	gtk_container_add(GTK_CONTAINER(ftmpw), hbox);
	
	/* Set button   */
	entry = gtk_entry_new();
	button = gtk_button_new_with_label("_select");
	g_signal_connect(G_OBJECT(button), "clicked", 
				G_CALLBACK(kemoview_gtk_PSFcolorsel), (gpointer)entry);
	gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);
	gtk_widget_show_all(ftmpw);
	gtk_main();
	return;
}


static void kemoview_gtk_BGcolorsel(GtkButton *button, gpointer data){
	int response;
	GtkWidget *parent;
	GtkEntry *entry;
	parent = GTK_WIDGET(g_object_get_data(G_OBJECT(data), "parent"));
	entry = GTK_ENTRY(data);
	
	rangew = gtk_color_selection_dialog_new("Choose color");
	gtk_widget_show_all(rangew);
	
	response = gtk_dialog_run(GTK_DIALOG(rangew));
	if (response == GTK_RESPONSE_OK){
		set_background_GTK(rangew);
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
	GtkWidget *hbox;
	GtkWidget *entry;
	GtkWidget *button;
	
	ftmpw = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	
	gtk_window_set_title(GTK_WINDOW(ftmpw), title);
	gtk_widget_set_size_request(ftmpw, 150, -1);
	gtk_container_set_border_width(GTK_CONTAINER(ftmpw), 5);
	g_signal_connect(G_OBJECT(ftmpw), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	
	/*hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);*/
	hbox = gtk_hbox_new(FALSE, 0);
	
	gtk_container_add(GTK_CONTAINER(ftmpw), hbox);
	
	/* Set button   */
	entry = gtk_entry_new();
	button = gtk_button_new_with_label("_select");
	g_signal_connect(G_OBJECT(button), "clicked", 
				G_CALLBACK(kemoview_gtk_BGcolorsel), (gpointer)entry);
	gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);
	gtk_widget_show_all(ftmpw);
	gtk_main();
	return;
}


static void gtk_nline_menu(int nline, const char *title){
	GtkWidget *box;
	GtkWidget *box1, *box2, *box3, *box5;
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
	char name[1024];
	double range_min, range_max;
	
	int ifield = kemoview_get_PSF_field_id();
	int icomp = kemoview_get_PSF_draw_data_address();
	
	kemoview_get_PSF_field_name(name, ifield);
	range_min = kemoview_get_PSF_min_data(icomp);
	range_max = kemoview_get_PSF_max_data(icomp);
	
	gtk_colormap_menu(range_min, range_max, name);
	
	if(iflag_set == MODIFY_POINT){
		kemoview_set_PSF_color_data(i_selected, gtk_value, gtk_color);}
	else if(iflag_set == ADD_POINT) {
		kemoview_add_PSF_color_list(gtk_value, gtk_color);}
	else if(iflag_set == DELETE_POINT) {
		kemoview_delete_PSF_color_list(i_selected);}
	
	draw_mesh_keep_menu();
	return;
}

void edit_psf_opasitymap_gtk(){
	char name[1024];
	double range_min, range_max;
	
	int ifield = kemoview_get_PSF_field_id();
	int icomp = kemoview_get_PSF_draw_data_address();
	
	kemoview_get_PSF_field_name(name, ifield);
	range_min = kemoview_get_PSF_min_data(icomp);
	range_max = kemoview_get_PSF_max_data(icomp);
	
	gtk_opacitymap_menu(range_min, range_max, name);
	
	if(iflag_set == MODIFY_POINT){
		kemoview_set_PSF_opacity_data(i_selected, gtk_value, gtk_opacity);}
	else if(iflag_set == ADD_POINT) {
		kemoview_add_PSF_opacity_list(gtk_value, gtk_opacity);}
	else if(iflag_set == DELETE_POINT) {
		kemoview_delete_PSF_opacity_list(i_selected);}
	
	draw_mesh_keep_menu();
	return;
}

void set_psf_range_gtk(){
	double range_min, range_max;
	char name[1024];
	
	int ifield = kemoview_get_PSF_field_id();
	int icomp = kemoview_get_PSF_draw_data_address();
	
	kemoview_get_PSF_field_name(name, ifield);
	range_min = kemoview_get_PSF_min_data(icomp);
	range_max = kemoview_get_PSF_max_data(icomp);
	
	gtk_range_menu(range_min, range_max, name);
	if(iflag_set == IZERO) return; 
	
	kemoview_set_PSF_linear_colormap(gtk_min, gtk_max);
	return;
}

void set_fline_range_gtk(){
	double range_min, range_max;
	char name[1024];
	
	int ifield = kemoview_get_fline_color_field();
	int icomp = kemoview_get_fline_color_data_adress();
	range_min = kemoview_get_fline_data_min(icomp);
	range_max = kemoview_get_fline_data_max(icomp);
	kemoview_get_fline_color_data_name(name, ifield);
	
	gtk_range_menu(range_min, range_max, name);
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

