/*
 *  kemoview_gtk_rotation_menu.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_quilt_menu.h"
#include "view_modifier_glfw.h"

struct quilt_gtk_menu * init_quilt_menu_box(void){
	struct quilt_gtk_menu *quilt_gmenu
			= (struct quilt_gtk_menu *)  malloc(sizeof(struct quilt_gtk_menu));
	quilt_gmenu->id_fmt_quilt = 0;
	
	quilt_gmenu->num_column = 5;
	quilt_gmenu->num_raw =    9;
	return quilt_gmenu;
};


static void quilt_switch_CB(GObject *switch_bar, GParamSpec *pspec, gpointer data)
{
    double FoculPoint;
    double eyeAngle, eyeRatio;
	struct quilt_gtk_menu *quilt_gmenu
			= (struct quilt_gtk_menu *) g_object_get_data(G_OBJECT(switch_bar), "parent");
	kemoview_toggle_quilt_flag(ISET_QUILT_MODE);
	int iflag_quilt = kemoview_get_quilt_nums(ISET_QUILT_MODE);
    if(iflag_quilt > 0){
		quilt_gmenu->num_column = 9;
		quilt_gmenu->num_raw = 5;
		FoculPoint = 9.5;
		eyeAngle = 35.0;
		kemoview_set_quilt_nums(ISET_QUILT_RAW, quilt_gmenu->num_raw);
		kemoview_set_quilt_nums(ISET_QUILT_COLUMN, quilt_gmenu->num_column);
		kemoview_set_stereo_parameter(ISET_FOCUS, FoculPoint);
		kemoview_set_stereo_parameter(ISET_EYEAGL, eyeAngle);
		
		eyeRatio = kemoview_get_view_parameter(ISET_EYESEP, 0);
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(quilt_gmenu->spin_num_column), 
								  quilt_gmenu->num_column);
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(quilt_gmenu->spin_num_raw), 
								  quilt_gmenu->num_raw);
		/*
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(view_menu->spin_eye_sep), FoculPoint);
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(view_menu->spin_eye_sep), eyeRatio);
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(view_menu->spin_sep_angle), eyeAngle);
		*/
	};
	return;
}

static void quilt_preview_CB(GtkButton *button, gpointer user_data){
	GtkEntry *entry = GTK_ENTRY(user_data);
	GtkWidget *window = GTK_WIDGET(g_object_get_data(G_OBJECT(user_data), "parent"));
	struct quilt_gtk_menu *quilt_gmenu 
			= (struct quilt_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "quilt");
    int i;
    i = kemoview_get_quilt_nums(ISET_QUILT_MODE);
	if(kemoview_get_quilt_nums(ISET_QUILT_MODE) == 0) return;
	
    int num_step = kemoview_get_quilt_nums(ISET_QUILT_NUM);
	for (i=0;i<num_step; i++){
        kemoview_set_quilt_nums(ISET_QUILT_COUNT, i);
		kemoview_set_view_integer(ISET_ROTATE_AXIS, IONE);
		kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO);
		draw_quilt();
	}
    draw_full();
	return;
};

static void num_quilt_column_CB(GtkWidget *entry, gpointer data){
	int gtk_intvalue = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
    kemoview_set_quilt_nums(ISET_QUILT_COLUMN, gtk_intvalue);
	return;
};
static void num_quilt_raw_CB(GtkWidget *entry, gpointer data){
	int gtk_intvalue = gtk_spin_button_get_value(GTK_SPIN_BUTTON(entry));
    kemoview_set_quilt_nums(ISET_QUILT_RAW, gtk_intvalue);
	return;
};


GtkWidget * init_quilt_menu_expander(struct quilt_gtk_menu *quilt_gmenu, GtkWidget *window){
	GtkWidget *expander_quilt;
	
	quilt_gmenu->entry_quilt_menu = gtk_entry_new();
	g_object_set_data(G_OBJECT(quilt_gmenu->entry_quilt_menu), "parent", (gpointer) window);
	g_object_set_data(G_OBJECT(quilt_gmenu->entry_quilt_menu), "quilt", (gpointer) quilt_gmenu);
	
	int iflag_quilt = kemoview_get_quilt_nums(ISET_QUILT_MODE);
	quilt_gmenu->quiltOn_Switch = gtk_switch_new();
	
	g_signal_connect(G_OBJECT(quilt_gmenu->quiltOn_Switch), "notify::active", 
					 G_CALLBACK(quilt_switch_CB), NULL);
	quilt_gmenu->quiltView_Button = gtk_button_new_with_label("Preview");
	g_signal_connect(G_OBJECT(quilt_gmenu->quiltView_Button), "clicked", 
					 G_CALLBACK(quilt_preview_CB), (gpointer)quilt_gmenu->entry_quilt_menu);
	
	
	GtkAdjustment *adj_num_column = gtk_adjustment_new(quilt_gmenu->num_column, 1, 30, 1, 1, 0.0);
	quilt_gmenu->spin_num_column = gtk_spin_button_new(GTK_ADJUSTMENT(adj_num_column), 0, 1);
	g_signal_connect(quilt_gmenu->spin_num_column, "value-changed",
					 G_CALLBACK(num_quilt_column_CB),NULL);
	GtkWidget *hbox_num_column = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_num_column), gtk_label_new("Num. of Columns: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_num_column), quilt_gmenu->spin_num_column, TRUE, TRUE, 0);
	
	
	GtkAdjustment *adj_num_raw = gtk_adjustment_new(quilt_gmenu->num_raw, 1, 30, 1, 1, 0.0);
	quilt_gmenu->spin_num_raw = gtk_spin_button_new(GTK_ADJUSTMENT(adj_num_raw), 0, 1);
	g_signal_connect(quilt_gmenu->spin_num_raw, "value-changed",
					 G_CALLBACK(num_quilt_raw_CB),NULL);
	
	GtkWidget *hbox_num_raw = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_num_raw), gtk_label_new("Num. of Columns: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_num_raw), quilt_gmenu->spin_num_raw, TRUE, TRUE, 0);
	
	
	GtkWidget *hbox_quilt_switch = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	if(iflag_quilt == 0){
		gtk_switch_set_state(quilt_gmenu->quiltOn_Switch, FALSE);
	} else {
		gtk_switch_set_state(quilt_gmenu->quiltOn_Switch, TRUE);
	};
	g_object_set_data(G_OBJECT(quilt_gmenu->quiltOn_Switch), 
					  "parent", (gpointer) quilt_gmenu);
	
	gtk_box_pack_start(GTK_BOX(hbox_quilt_switch), gtk_label_new("Quilt mode: "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_quilt_switch), quilt_gmenu->quiltOn_Switch, FALSE, FALSE, 0);
	
	GtkWidget *vbox_quilt_switch = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(vbox_quilt_switch), hbox_quilt_switch, TRUE, FALSE, 0);
	
	GtkWidget *hbox_quilt_preview = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 10);
	gtk_box_pack_start(GTK_BOX(hbox_quilt_preview), vbox_quilt_switch, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_quilt_preview), quilt_gmenu->quiltView_Button, FALSE, FALSE, 0);
	
	GtkWidget *quilt_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(quilt_box), hbox_quilt_preview, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(quilt_box), hbox_num_column, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(quilt_box), hbox_num_raw, FALSE, TRUE, 0);
	
	expander_quilt = wrap_into_scroll_expansion_gtk("Quilt", 200, 120, window, quilt_box);
	return expander_quilt;
}
