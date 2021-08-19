/*
 *  kemoview_gtk_viewmatrix_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#include "kemoview_gtk_viewmatrix_menu.h"


void update_windowsize_menu(struct view_widgets *view_menu, GtkWidget *window){
    char windowsize_x_text[30];
    char windowsize_y_text[30];
    
    sprintf(windowsize_x_text, "    %d    ", kemoview_get_view_integer(ISET_PIXEL_X));
    sprintf(windowsize_y_text, "    %d    ", kemoview_get_view_integer(ISET_PIXEL_Y));
    
    gtk_label_set_text(GTK_LABEL(view_menu->spin_win_x), windowsize_x_text);
    gtk_label_set_text(GTK_LABEL(view_menu->spin_win_y), windowsize_y_text);
	
	gtk_widget_queue_draw(window);
	draw_full();
	return;
};

void set_viewmatrix_value(struct view_widgets *view_menu, GtkWidget *window){	
	gtk_adjustment_set_value(view_menu->adj_eye_x, -kemoview_get_view_parameter(ISET_SHIFT, 0));
	gtk_adjustment_set_value(view_menu->adj_eye_y, -kemoview_get_view_parameter(ISET_SHIFT, 1));
	gtk_adjustment_set_value(view_menu->adj_eye_z, -kemoview_get_view_parameter(ISET_SHIFT, 2));
	
	gtk_adjustment_set_value(view_menu->adj_scale, kemoview_get_view_parameter(ISET_SCALE, 0));
	
	gtk_adjustment_set_value(view_menu->adj_rotation_x, kemoview_get_view_parameter(ISET_ROTATE, 1));
	gtk_adjustment_set_value(view_menu->adj_rotation_y, kemoview_get_view_parameter(ISET_ROTATE, 2));
	gtk_adjustment_set_value(view_menu->adj_rotation_z, kemoview_get_view_parameter(ISET_ROTATE, 3));
	gtk_adjustment_set_value(view_menu->adj_rotation_deg, kemoview_get_view_parameter(ISET_ROTATE, 0));
	
	gtk_adjustment_set_value(view_menu->adj_aperture, kemoview_get_view_parameter(ISET_APERTURE, 0));
	
	gtk_adjustment_set_value(view_menu->adj_focus, kemoview_get_view_parameter(ISET_FOCUS, 0));
	gtk_adjustment_set_value(view_menu->adj_eye_sep, kemoview_get_view_parameter(ISET_EYESEP, 0));
    return;
};

void update_viewmatrix_menu(struct view_widgets *view_menu, GtkWidget *window){    
	if(kemoview_get_view_type_flag() != VIEW_STEREO){
        gtk_widget_hide(view_menu->Frame_streo);
    };
    gtk_widget_queue_draw(window);
    return;
};


static void save_viewmatrix_CB(GtkButton *button, gpointer user_data){
	int iflag_set = kemoview_gtk_save_file_select(button, user_data);
	if(iflag_set == IZERO) return;
	
	GtkEntry *entry = GTK_ENTRY(user_data);
	struct kv_string *filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
	
	kemoview_write_modelview_file(filename);
	kemoview_free_kvstring(filename);
	
	return;
};

static void load_viewmatrix_CB(GtkButton *button, gpointer user_data){
	
	int iflag_set = kemoview_gtk_read_file_select(button, user_data);
	
	if(iflag_set == IZERO) return;
	GtkEntry *entry = GTK_ENTRY(user_data);
	struct kv_string *filename = kemoview_init_kvstring_by_string(gtk_entry_get_text(entry));
	kemoview_load_modelview_file(filename);
	kemoview_free_kvstring(filename);
	
	draw_full();
	return;
};

static void eye_position_x_CB(GtkWidget *entry, gpointer user_data){
	int *iflag_fast_draw = (int *) user_data;
	double gtk_floatvalue = -gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_view_parameter(ISET_SHIFT, 0, gtk_floatvalue);
	
	*iflag_fast_draw = draw_fast();
	return;
};
static void eye_position_y_CB(GtkWidget *entry, gpointer user_data){
	int *iflag_fast_draw = (int *) user_data;
	double gtk_floatvalue = -gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_view_parameter(ISET_SHIFT, 1, gtk_floatvalue);
	
	*iflag_fast_draw = draw_fast();
	return;
};
static void eye_position_z_CB(GtkWidget *entry, gpointer user_data){
	int *iflag_fast_draw = (int *) user_data;
	double gtk_floatvalue = -gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_view_parameter(ISET_SHIFT, 2, gtk_floatvalue);
	
	*iflag_fast_draw = draw_fast();
	return;
};

static void scale_CB(GtkWidget *entry, gpointer user_data){
	int *iflag_fast_draw = (int *) user_data;
	double gtk_floatvalue = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_view_parameter(ISET_SCALE, 0, gtk_floatvalue);
	
	*iflag_fast_draw = draw_fast();
	return;
};

static void spin_x_CB(GtkWidget *entry, gpointer user_data){
	int *iflag_fast_draw = (int *) user_data;
	double gtk_floatvalue = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_view_parameter(ISET_ROTATE, 1, gtk_floatvalue);
	
	*iflag_fast_draw = draw_fast();
	return;
};
static void spin_y_CB(GtkWidget *entry, gpointer user_data){
	int *iflag_fast_draw = (int *) user_data;
	double gtk_floatvalue = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_view_parameter(ISET_ROTATE, 2, gtk_floatvalue);
	
	*iflag_fast_draw = draw_fast();
	return;
};
static void spin_z_CB(GtkWidget *entry, gpointer user_data){
	int *iflag_fast_draw = (int *) user_data;
	double gtk_floatvalue = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_view_parameter(ISET_ROTATE, 3, gtk_floatvalue);
	
	*iflag_fast_draw = draw_fast();
	return;
};
static void spin_deg_CB(GtkWidget *entry, gpointer user_data){
	int *iflag_fast_draw = (int *) user_data;
	double gtk_floatvalue = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_view_parameter(ISET_ROTATE, 0, gtk_floatvalue);
	
	*iflag_fast_draw = draw_fast();
	return;
};

static void aperture_CB(GtkWidget *entry, gpointer user_data){
	int *iflag_fast_draw = (int *) user_data;
	double gtk_floatvalue = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_view_parameter(ISET_APERTURE, 0, gtk_floatvalue);
	
	*iflag_fast_draw = draw_fast();
	return;
};

static void focus_CB(GtkWidget *entry, gpointer user_data){
	int *iflag_fast_draw = (int *) user_data;
	double gtk_floatvalue = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_stereo_parameter(ISET_FOCUS, gtk_floatvalue);
	
	*iflag_fast_draw = draw_fast();
	return;
};
static void eye_sep_CB(GtkWidget *entry, gpointer user_data){
	int *iflag_fast_draw = (int *) user_data;
	double gtk_floatvalue = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
	kemoview_set_stereo_parameter(ISET_EYESEP, gtk_floatvalue);
	
	*iflag_fast_draw = draw_fast();
	return;
};

GtkWidget * init_viewmatrix_menu_expander(int *iflag_fast_draw, struct view_widgets *view_menu,
                                          GtkWidget *window){
    GtkWidget *expander_view;
	
    char windowsize_x_text[30];
    char windowsize_y_text[30];
    
	char current_lookat_x_text[30];
	char current_lookat_y_text[30];
	char current_lookat_z_text[30];
	
    sprintf(windowsize_x_text, "    %d    ", kemoview_get_view_integer(ISET_PIXEL_X));
    sprintf(windowsize_y_text, "    %d    ", kemoview_get_view_integer(ISET_PIXEL_Y));
    
	sprintf(current_lookat_x_text, "    %f    ",
			(float) kemoview_get_view_parameter(ISET_VWPOINT, 0));
	sprintf(current_lookat_y_text, "    %f    ",
			(float) kemoview_get_view_parameter(ISET_VWPOINT, 1));
	sprintf(current_lookat_z_text, "    %f    ",
			(float) kemoview_get_view_parameter(ISET_VWPOINT, 2));
	
    GtkWidget *entry = gtk_entry_new();
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer) window);
	g_object_set_data(G_OBJECT(entry), "menu", (gpointer) view_menu);
	
    view_menu->spin_win_x = gtk_label_new(windowsize_x_text);
    view_menu->spin_win_y = gtk_label_new(windowsize_y_text);
    
	view_menu->adj_eye_x = gtk_adjustment_new(0.0, -1000.0, 1000.0, 1., 1., 0.0);
	view_menu->adj_eye_y = gtk_adjustment_new(0.0, -1000.0, 1000.0, 1., 1., 0.0);
	view_menu->adj_eye_z = gtk_adjustment_new(0.0, -1000.0, 1000.0, 1., 1., 0.0);
	
	view_menu->adj_scale = gtk_adjustment_new(0.0, 0.0, 1000.0, 0.01, 0.01, 0.0);
	
	view_menu->adj_rotation_x = gtk_adjustment_new(0.0, -1.0, 1.0, 0.01, 0.01, 0.0);
	view_menu->adj_rotation_y = gtk_adjustment_new(0.0, -1.0, 1.0, 0.01, 0.01, 0.0);
	view_menu->adj_rotation_z = gtk_adjustment_new(0.0, -1.0, 1.0, 0.01, 0.01, 0.0);
	view_menu->adj_rotation_deg = gtk_adjustment_new(0.0, -180.0, 360.0, 1, 1, 0.0);
	
	view_menu->adj_aperture = gtk_adjustment_new(0.0, 0.0, 180.0, 0.01, 0.01, 0.0);
	
	view_menu->adj_focus = gtk_adjustment_new(0.0, 0.0, 1000.0, 0.01, 0.01, 0.0);
	view_menu->adj_eye_sep = gtk_adjustment_new(0.0, 0.0, 100.0, 0.01, 0.01, 0.0);
	
	update_windowsize_menu(view_menu, window);
	set_viewmatrix_value(view_menu, window);
	
	view_menu->spin_eye_x = gtk_spin_button_new(GTK_ADJUSTMENT(view_menu->adj_eye_x), 0, 3);
	g_signal_connect(view_menu->spin_eye_x, "value-changed", G_CALLBACK(eye_position_x_CB), (gpointer) iflag_fast_draw);
	view_menu->spin_eye_y = gtk_spin_button_new(GTK_ADJUSTMENT(view_menu->adj_eye_y), 0, 3);
	g_signal_connect(view_menu->spin_eye_y, "value-changed", G_CALLBACK(eye_position_y_CB), (gpointer) iflag_fast_draw);
	view_menu->spin_eye_z = gtk_spin_button_new(GTK_ADJUSTMENT(view_menu->adj_eye_z), 0, 3);
	g_signal_connect(view_menu->spin_eye_z, "value-changed", G_CALLBACK(eye_position_z_CB), (gpointer) iflag_fast_draw);
	
	view_menu->spin_scale = gtk_spin_button_new(GTK_ADJUSTMENT(view_menu->adj_scale), 0, 3);
	g_signal_connect(view_menu->spin_scale, "value-changed", G_CALLBACK(scale_CB), (gpointer) iflag_fast_draw);
	view_menu->spin_rotation_x = gtk_spin_button_new(GTK_ADJUSTMENT(view_menu->adj_rotation_x), 0, 3);
	g_signal_connect(view_menu->spin_rotation_x, "value-changed", G_CALLBACK(spin_x_CB), (gpointer) iflag_fast_draw);
	view_menu->spin_rotation_y = gtk_spin_button_new(GTK_ADJUSTMENT(view_menu->adj_rotation_y), 0, 3);
	g_signal_connect(view_menu->spin_rotation_y, "value-changed", G_CALLBACK(spin_y_CB), (gpointer) iflag_fast_draw);
	view_menu->spin_rotation_z = gtk_spin_button_new(GTK_ADJUSTMENT(view_menu->adj_rotation_z), 0, 3);
	g_signal_connect(view_menu->spin_rotation_z, "value-changed", G_CALLBACK(spin_z_CB), (gpointer) iflag_fast_draw);
	view_menu->spin_rotation_deg = gtk_spin_button_new(GTK_ADJUSTMENT(view_menu->adj_rotation_deg), 0, 3);
	g_signal_connect(view_menu->spin_rotation_deg, "value-changed", G_CALLBACK(spin_deg_CB), (gpointer) iflag_fast_draw);
	
	view_menu->spin_aperture = gtk_spin_button_new(GTK_ADJUSTMENT(view_menu->adj_aperture), 0, 3);
	g_signal_connect(view_menu->spin_aperture, "value-changed", G_CALLBACK(aperture_CB), (gpointer) iflag_fast_draw);
	
	
	view_menu->hbox_win_x = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_win_x), gtk_label_new(" X: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_win_x), view_menu->spin_win_x, FALSE, FALSE, 0);
	
	view_menu->hbox_win_y = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_win_y), gtk_label_new(" Y: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_win_y), view_menu->spin_win_y, FALSE, FALSE, 0);
	
	view_menu->vbox_win = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->vbox_win), view_menu->hbox_win_x, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->vbox_win), view_menu->hbox_win_y, FALSE, FALSE, 0);
	view_menu->Frame_win = gtk_frame_new("Number of pixel");
	gtk_frame_set_shadow_type(GTK_FRAME(view_menu->Frame_win), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(view_menu->Frame_win), view_menu->vbox_win);
	
	view_menu->hbox_eye_x = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_eye_x), gtk_label_new(" X: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_eye_x), view_menu->spin_eye_x, FALSE, FALSE, 0);
	
	view_menu->hbox_eye_y = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_eye_y), gtk_label_new(" Y: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_eye_y), view_menu->spin_eye_y, FALSE, FALSE, 0);
	
	view_menu->hbox_eye_z = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_eye_z), gtk_label_new(" Z: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_eye_z), view_menu->spin_eye_z, FALSE, FALSE, 0);
	
	view_menu->vbox_eye = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->vbox_eye), view_menu->hbox_eye_x, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->vbox_eye), view_menu->hbox_eye_y, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->vbox_eye), view_menu->hbox_eye_z, FALSE, TRUE, 0);
	view_menu->Frame_eye = gtk_frame_new("Eye position");
	gtk_frame_set_shadow_type(GTK_FRAME(view_menu->Frame_eye), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(view_menu->Frame_eye), view_menu->vbox_eye);
	
	
	
	view_menu->hbox_looking_x = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_looking_x), gtk_label_new(" X: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_looking_x), gtk_label_new(current_lookat_x_text), FALSE, FALSE, 0);
	
	view_menu->hbox_looking_y = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_looking_y), gtk_label_new(" Y: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_looking_y), gtk_label_new(current_lookat_y_text), FALSE, FALSE, 0);
	
	view_menu->hbox_looking_z = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_looking_z), gtk_label_new(" Z: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_looking_z), gtk_label_new(current_lookat_z_text), FALSE, FALSE, 0);
	
	view_menu->vbox_looking = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->vbox_looking), view_menu->hbox_looking_x, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->vbox_looking), view_menu->hbox_looking_y, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->vbox_looking), view_menu->hbox_looking_z, FALSE, FALSE, 0);
	view_menu->Frame_looking = gtk_frame_new("Look at position");
	gtk_frame_set_shadow_type(GTK_FRAME(view_menu->Frame_looking), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(view_menu->Frame_looking), view_menu->vbox_looking);
	
	
	view_menu->hbox_scale = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_scale), gtk_label_new("Scale: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_scale), view_menu->spin_scale, FALSE, FALSE, 0);
	
	view_menu->vbox_scale = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->vbox_scale), view_menu->hbox_scale, FALSE, FALSE, 0);
	view_menu->Frame_scale = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(view_menu->Frame_scale), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(view_menu->Frame_scale), view_menu->vbox_scale);
	
	
	
	view_menu->hbox_rotation_x = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_rotation_x), gtk_label_new(" X: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_rotation_x), view_menu->spin_rotation_x, FALSE, FALSE, 0);
	
	view_menu->hbox_rotation_y = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_rotation_y), gtk_label_new(" Y: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_rotation_y), view_menu->spin_rotation_y, FALSE, FALSE, 0);
	
	view_menu->hbox_rotation_z = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_rotation_z), gtk_label_new(" Z: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_rotation_z), view_menu->spin_rotation_z, FALSE, FALSE, 0);
	
	view_menu->hbox_rotation_deg = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_rotation_deg), gtk_label_new(" Angle (Deg.): "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_rotation_deg), view_menu->spin_rotation_deg, FALSE, FALSE, 0);
	
	view_menu->vbox_rotation = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->vbox_rotation), view_menu->hbox_rotation_x, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->vbox_rotation), view_menu->hbox_rotation_y, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->vbox_rotation), view_menu->hbox_rotation_z, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->vbox_rotation), view_menu->hbox_rotation_deg, FALSE, TRUE, 0);
	view_menu->Frame_rotation = gtk_frame_new("Rotation");
	gtk_frame_set_shadow_type(GTK_FRAME(view_menu->Frame_rotation), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(view_menu->Frame_rotation), view_menu->vbox_rotation);
	
	
	
	view_menu->hbox_aperture = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_aperture), gtk_label_new(" Aperture: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_aperture), view_menu->spin_aperture, FALSE, FALSE, 0);
	
	view_menu->vbox_aperture = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->vbox_aperture), view_menu->hbox_aperture, FALSE, TRUE, 0);
	view_menu->Frame_aperture = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(view_menu->Frame_aperture), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(view_menu->Frame_aperture), view_menu->vbox_aperture);
	
	
	view_menu->entry_viewmatrix_file = gtk_entry_new();
	g_object_set_data(G_OBJECT(view_menu->entry_viewmatrix_file),
				"parent", (gpointer) window);
	view_menu->saveView_Button = gtk_button_new_with_label("Save View...");
	g_signal_connect(G_OBJECT(view_menu->saveView_Button), "clicked", 
					 G_CALLBACK(save_viewmatrix_CB), (gpointer)view_menu->entry_viewmatrix_file);
	view_menu->loadView_Button = gtk_button_new_with_label("Load View...");
	g_signal_connect(G_OBJECT(view_menu->loadView_Button), "clicked", 
					 G_CALLBACK(load_viewmatrix_CB), (gpointer)view_menu->entry_viewmatrix_file);
	
	view_menu->hbox_viewmatrix_save = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_viewmatrix_save), view_menu->saveView_Button, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_viewmatrix_save), view_menu->loadView_Button, FALSE, FALSE, 0);

	
	GtkWidget *box_view = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(box_view), view_menu->Frame_win, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box_view), view_menu->Frame_eye, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box_view), view_menu->Frame_looking, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box_view), view_menu->Frame_rotation, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(box_view), view_menu->Frame_aperture, FALSE, FALSE, 0);
	
	view_menu->spin_focus = gtk_spin_button_new(GTK_ADJUSTMENT(view_menu->adj_focus), 0, 3);
	g_signal_connect(view_menu->spin_focus, "value-changed", G_CALLBACK(focus_CB), (gpointer) iflag_fast_draw);
	view_menu->spin_eye_sep =   gtk_spin_button_new(GTK_ADJUSTMENT(view_menu->adj_eye_sep), 0, 3);
	g_signal_connect(view_menu->spin_eye_sep, "value-changed", G_CALLBACK(eye_sep_CB), (gpointer) iflag_fast_draw);
	
	view_menu->hbox_focus = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_focus), gtk_label_new(" Focus: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_focus), view_menu->spin_focus, FALSE, FALSE, 0);
	
	view_menu->hbox_eye_sep = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_eye_sep), gtk_label_new(" Eye separation: "), TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->hbox_eye_sep), view_menu->spin_eye_sep, FALSE, FALSE, 0);
	
	view_menu->vbox_streo = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->vbox_streo), view_menu->hbox_focus, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(view_menu->vbox_streo), view_menu->hbox_eye_sep, FALSE, FALSE, 0);
	view_menu->Frame_streo = gtk_frame_new("Stereo parameter");
	gtk_frame_set_shadow_type(GTK_FRAME(view_menu->Frame_streo), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(view_menu->Frame_streo), view_menu->vbox_streo);
	
	gtk_box_pack_start(GTK_BOX(box_view), view_menu->Frame_streo, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(box_view), view_menu->hbox_viewmatrix_save, FALSE, FALSE, 0);
    expander_view = wrap_into_expanded_frame_gtk("View parameters", 360, 480, window, box_view);
    return expander_view;
}

