/*
//  control_panel_real_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_real_GTK.h"

static void copy_f_ctl_r_array_by_r_list(struct real_clist *r_clist)
{
	int i;
	for(i=0;i<count_real_clist(r_clist);i++){
		c_store_real_array(r_clist->f_self, i, 
						   real_clist_at_index(i, r_clist)->r_data);
	}
    return;
}

void update_f_ctl_r_array_by_r_list(struct real_clist *r_clist)
{
/*	c_check_real_array(r_clist->f_self);*/
	copy_f_ctl_r_array_by_r_list(r_clist);
/*	c_check_real_array(r_clist->f_self);*/
    return;
}
void reflesh_f_ctl_r_array_by_r_list(struct real_clist *r_clist)
{
/*	c_check_real_array(r_clist->f_self);*/
	int num_array = count_real_clist(r_clist);
    printf("num_array %d \n", num_array);
	reflesh_f_ctl_real_array(num_array, r_clist);
    printf("reflesh_f_ctl_real_array %d \n", num_array);
	copy_f_ctl_r_array_by_r_list(r_clist);
/*	c_check_real_array(r_clist->f_self);*/
    return;
}

/* Append new data at the end of list */

void cb_check_r_array_toggle(GtkWidget *widget, gpointer user_data){
	GtkToggleButton *toggle = GTK_TOGGLE_BUTTON(widget);
	struct real_clist *r_clist_gtk = (struct real_clist *) g_object_get_data(G_OBJECT(widget), "real_clist");
	
	if(gtk_toggle_button_get_active(toggle) == TRUE){
		reflesh_f_ctl_real_array(count_real_clist(r_clist_gtk), r_clist_gtk);
		copy_f_ctl_r_array_by_r_list(r_clist_gtk);
	}else{
		reflesh_f_ctl_real_array(0, r_clist_gtk);
	};
	return;
}

GtkWidget *hbox_with_r_array_checkbox(struct real_clist *r_clist_gtk){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	GtkWidget *checkbox = gtk_check_button_new();
	g_object_set_data(G_OBJECT(checkbox), "real_clist", (gpointer) r_clist_gtk);
	
	if(c_real_array_num(r_clist_gtk->f_self) == 0){
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), FALSE);
	} else {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), TRUE);
	}
	
	g_signal_connect(G_OBJECT(checkbox), "toggled",
                     G_CALLBACK(cb_check_r_array_toggle), NULL);
	
	gtk_box_pack_start(GTK_BOX(hbox), checkbox, TRUE, TRUE, 0);
	return hbox;
}


static void r_tree_value1_edited_cb(GtkCellRendererText *cell, gchar *path_str,
									gchar *new_text, gpointer user_data){
	GtkWidget *real_array_tree_view = GTK_WIDGET(user_data);
	struct real_clist *r_clist_gtk = (struct real_clist *) g_object_get_data(G_OBJECT(cell), "real_clist");
	r_tree_value1_edited(path_str, new_text, GTK_TREE_VIEW(real_array_tree_view), r_clist_gtk);
	update_f_ctl_r_array_by_r_list(r_clist_gtk);
};

static void add_r_list_items_cb(GtkButton *button, gpointer user_data){
	GtkWidget *real_array_tree_view = GTK_WIDGET(user_data);
	struct real_clist *r_clist_gtk = (struct real_clist *) g_object_get_data(G_OBJECT(button), "real_clist");
	r_clist_gtk->index_bc = add_r_list_items(GTK_TREE_VIEW(real_array_tree_view), r_clist_gtk);
	reflesh_f_ctl_r_array_by_r_list(r_clist_gtk);
};

static void delete_r_list_items_cb(GtkButton *button, gpointer user_data){
	GtkWidget *real_array_tree_view = GTK_WIDGET(user_data);
	struct real_clist *r_clist_gtk = (struct real_clist *) g_object_get_data(G_OBJECT(button), "real_clist");
	delete_r_list_items(GTK_TREE_VIEW(real_array_tree_view), r_clist_gtk);
	reflesh_f_ctl_r_array_by_r_list(r_clist_gtk);
/*    write_real_clist(stdout, 0, "columns deleted", r_clist_gtk); */
};

static void init_real_tree_view(struct real_clist *r_clist_gtk, GtkWidget *real_array_tree_view){
	GtkCellRenderer *renderer_spin1 = gtk_cell_renderer_text_new();
	g_object_set_data(G_OBJECT(renderer_spin1), "real_clist", (gpointer) r_clist_gtk);
	g_signal_connect(G_OBJECT(renderer_spin1), "edited", 
					 G_CALLBACK(r_tree_value1_edited_cb), (gpointer) real_array_tree_view);
	
	create_real_tree_view(GTK_TREE_VIEW(real_array_tree_view), r_clist_gtk, 
                           renderer_spin1);
	r_clist_gtk->index_bc = append_r_list_from_ctl(r_clist_gtk->index_bc, &r_clist_gtk->r_item_head,
												   GTK_TREE_VIEW(real_array_tree_view));
};


GtkWidget * real_array_vbox_w_addbottun(struct real_clist *r_clist_gtk, 
										GtkWidget *real_array_tree_view){
	GtkWidget * vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	real_array_tree_view = gtk_tree_view_new();
	init_real_tree_view(r_clist_gtk, real_array_tree_view);
	
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	g_object_set_data(G_OBJECT(button_add), "real_clist", (gpointer) r_clist_gtk);
	g_object_set_data(G_OBJECT(button_delete), "real_clist", (gpointer) r_clist_gtk);
	
	GtkWidget * hbox1 = hbox_with_r_array_checkbox(r_clist_gtk);
	GtkWidget *vbox0 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox0), hbox1, FALSE, TRUE, 0);
	
	GtkWidget * hbox0 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	
	gtk_box_pack_start(GTK_BOX(hbox0), vbox0, FALSE, TRUE, 0);
	GtkWidget *expander = real_list_box_expander(r_clist_gtk->clist_name, real_array_tree_view, 
												 button_add, button_delete);
	gtk_box_pack_start(GTK_BOX(hbox0), expander, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox0, FALSE, TRUE, 0);
	
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(add_r_list_items_cb), (gpointer) real_array_tree_view);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_r_list_items_cb), (gpointer) real_array_tree_view);
	return vbox;
};
