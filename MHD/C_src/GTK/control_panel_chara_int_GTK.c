/*
//  control_panel_chara_int_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_chara_int_GTK.h"

static void copy_f_ctl_ci_array_by_r_list(struct chara_int_clist *ci_clist)
{
	char *ctmp;
	int i;
	
	for(i=0;i<count_chara_int_clist(ci_clist);i++){
		ctmp = chara_int_clist_at_index(i, ci_clist)->c_tbl;
		c_store_chara_int_array(ci_clist->f_self, i, ctmp,
								chara_int_clist_at_index(i, ci_clist)->i_data);
	}
    return;
}

static void update_f_ctl_ci_array_by_r_list(struct chara_int_clist *ci_clist)
{
/*	c_check_chara_int_array(ci_clist->f_self);*/
	copy_f_ctl_ci_array_by_r_list(ci_clist);
/*	c_check_chara_int_array(ci_clist->f_self);*/
    return;
}
static void reflesh_f_ctl_ci_array_by_r_list(struct chara_int_clist *ci_clist)
{
/*	c_check_chara_int_array(ci_clist->f_self);*/
	int num_array = count_chara_int_clist(ci_clist);
	reflesh_f_ctl_ci_array(num_array, ci_clist);
	copy_f_ctl_ci_array_by_r_list(ci_clist);
/*	c_check_chara_int_array(ci_clist->f_self);*/
    return;
}


void cb_check_ci_array_toggle(GtkWidget *widget, gpointer user_data){
	GtkToggleButton *toggle = GTK_TOGGLE_BUTTON(widget);
	struct chara_int_clist *ci_clist = (struct chara_int_clist *) g_object_get_data(G_OBJECT(widget), "ci_clist");
	if(gtk_toggle_button_get_active(toggle) == TRUE){
		reflesh_f_ctl_ci_array(count_chara_int_clist(ci_clist), ci_clist);
		copy_f_ctl_ci_array_by_r_list(ci_clist);
	}else{
		reflesh_f_ctl_ci_array(0, ci_clist);
	};
	return;
}

static void add_ci_list_items_cb(GtkButton *button, gpointer user_data){
	GtkWidget *ci_tree_view = GTK_WIDGET(user_data);
	struct chara_int_clist *ci_clist = (struct chara_int_clist *) g_object_get_data(G_OBJECT(button), "ci_clist");
	printf("ci_clist %p %p \n",ci_clist, ci_tree_view);
	ci_clist->index_bc = add_ci_list_items_GTK(GTK_TREE_VIEW(ci_tree_view), ci_clist);
	printf("ci_clist->index_bc %d \n", ci_clist->index_bc);
	reflesh_f_ctl_ci_array_by_r_list(ci_clist);
	printf("ci_clist %p \n",ci_clist->f_self);
};

static void delete_ci_list_items_cb(GtkButton *button, gpointer user_data){
    GtkWidget *ci_tree_view = GTK_WIDGET(user_data);
	struct chara_int_clist *ci_clist = (struct chara_int_clist *) g_object_get_data(G_OBJECT(button), "ci_clist");
	delete_ci_list_items_GTK(GTK_TREE_VIEW(ci_tree_view), ci_clist);
	reflesh_f_ctl_ci_array_by_r_list(ci_clist);
};


static void ci_tree_value1_edited_cb(GtkCellRenderer *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    GtkWidget *ci_tree_view = GTK_WIDGET(user_data);
	struct chara_int_clist *ci_clist = (struct chara_int_clist *) g_object_get_data(G_OBJECT(cell), "ci_clist");
    ci_tree_name_edited(path_str, new_text, GTK_TREE_VIEW(ci_tree_view), ci_clist);
	update_f_ctl_ci_array_by_r_list(ci_clist);
};

static void ci_tree_value2_edited_cb(GtkCellRenderer *cell, gchar *path_str,
			gchar *new_text, gpointer user_data)
{
    GtkWidget *ci_tree_view = GTK_WIDGET(user_data);
	struct chara_int_clist *ci_clist = (struct chara_int_clist *) g_object_get_data(G_OBJECT(cell), "ci_clist");
    ci_tree_value_edited(path_str, new_text, GTK_TREE_VIEW(ci_tree_view), ci_clist);
	update_f_ctl_ci_array_by_r_list(ci_clist);
};


GtkWidget *hbox_with_ci_array_checkbox(struct chara_int_clist *ci_clist){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	GtkWidget *checkbox = gtk_check_button_new();
	g_object_set_data(G_OBJECT(checkbox), "ci_clist", (gpointer) ci_clist);
	
	if(count_chara_int_clist(ci_clist) == 0){
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), FALSE);
	} else {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), TRUE);
	}
	
	g_signal_connect(G_OBJECT(checkbox), "toggled",
                     G_CALLBACK(cb_check_ci_array_toggle), NULL);
	
	gtk_box_pack_start(GTK_BOX(hbox), checkbox, TRUE, TRUE, 0);
	return hbox;
}


static void init_ci_tree_view(struct chara_int_clist *ci_clist, GtkWidget *ci_tree_view){
	GtkCellRenderer *renderer_text;
	GtkCellRenderer *renderer_value;
	
	renderer_text =  gtk_cell_renderer_text_new();
	renderer_value = gtk_cell_renderer_text_new();
	g_object_set_data(G_OBJECT(renderer_text), "ci_clist", (gpointer) ci_clist);
	g_object_set_data(G_OBJECT(renderer_value), "ci_clist", (gpointer) ci_clist);
	
	g_signal_connect(G_OBJECT(renderer_text), "edited", 
					 G_CALLBACK(ci_tree_value1_edited_cb), (gpointer) ci_tree_view);
	g_signal_connect(G_OBJECT(renderer_value), "edited", 
					 G_CALLBACK(ci_tree_value2_edited_cb), (gpointer) ci_tree_view);
	
	create_text_int_tree_view(ci_clist, GTK_TREE_VIEW(ci_tree_view), 
							  renderer_text, renderer_value);
	
	ci_clist->index_bc = append_ci_list_from_ctl(ci_clist->index_bc, &ci_clist->ci_item_head, 
												 GTK_TREE_VIEW(ci_tree_view));
};

GtkWidget * add_ci_list_box_w_addbottun(struct chara_int_clist *ci_clist,
                                        GtkWidget *ci_tree_view){
	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	ci_tree_view = gtk_tree_view_new();
	init_ci_tree_view(ci_clist, ci_tree_view);
	
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	g_object_set_data(G_OBJECT(button_add), "ci_clist", (gpointer) ci_clist);
	g_object_set_data(G_OBJECT(button_delete), "ci_clist", (gpointer) ci_clist);
	
	GtkWidget * hbox1 = hbox_with_ci_array_checkbox(ci_clist);
	GtkWidget *vbox0 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox0), hbox1, FALSE, TRUE, 0);
	
	GtkWidget *hbox0 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	
	gtk_box_pack_start(GTK_BOX(hbox0), vbox0, FALSE, TRUE, 0);
	GtkWidget *expander = ci_list_box_expander(ci_clist->clist_name, GTK_TREE_VIEW(ci_tree_view), 
											   button_add, button_delete);
	gtk_box_pack_start(GTK_BOX(hbox0), expander, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox0, FALSE, TRUE, 0);
	
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(add_ci_list_items_cb), (gpointer) ci_tree_view);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_ci_list_items_cb), (gpointer) ci_tree_view);
	return vbox;
};

