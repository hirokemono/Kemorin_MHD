/*
//  control_panel_real_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_real_GTK.h"

static void copy_f_ctl_r_array_by_r_list(struct real_clist *r_clist,
										  struct f_ctl_real_array *f_r_array)
{
	int i;
	for(i=0;i<f_r_array->f_num[0];i++){
		f_r_array->f_rctls[i] = real_clist_at_index(i, r_clist)->r_data;
	}
    return;
}

void update_f_ctl_r_array_by_r_list(struct real_clist *r_clist,
									   struct f_ctl_real_array *f_r_array)
{
/*	c_check_real_array(f_r_array->f_self);*/
	copy_f_ctl_r_array_by_r_list(r_clist, f_r_array);
/*	c_check_real_array(f_r_array->f_self);*/
    return;
}
void reflesh_f_ctl_r_array_by_r_list(struct real_clist *r_clist,
									   struct f_ctl_real_array *f_r_array)
{
/*	c_check_real_array(f_r_array->f_self);*/
	int num_array = count_real_clist(r_clist);
	reflesh_f_ctl_real_array(num_array, f_r_array);
	copy_f_ctl_r_array_by_r_list(r_clist, f_r_array);
/*	c_check_real_array(f_r_array->f_self);*/
    return;
}

/* Append new data at the end of list */

void cb_check_r_array_toggle(GtkWidget *widget, gpointer user_data){
	GtkToggleButton *toggle = GTK_TOGGLE_BUTTON(widget);
	struct r_clist_view *r_vws = (struct r_clist_view *) g_object_get_data(G_OBJECT(widget), "r_clist_view");
	struct f_ctl_real_array *f_r_array = (struct f_ctl_real_array *) g_object_get_data(G_OBJECT(widget), "f_r_array");
	
	if(gtk_toggle_button_get_active(toggle) == TRUE){
		reflesh_f_ctl_real_array(count_real_clist(r_vws->r_clist_gtk), f_r_array);
		copy_f_ctl_r_array_by_r_list(r_vws->r_clist_gtk, f_r_array);
	}else{
		reflesh_f_ctl_real_array(0, f_r_array);
	};
	return;
}

GtkWidget *hbox_with_r_array_checkbox(struct f_ctl_real_array *f_r_array, struct r_clist_view *r_vws){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	GtkWidget *checkbox = gtk_check_button_new();
	g_object_set_data(G_OBJECT(checkbox), "r_clist_view", (gpointer) r_vws);
	g_object_set_data(G_OBJECT(checkbox), "f_r_array", (gpointer) f_r_array);
	
	if(f_r_array->f_num[0] == 0){
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
	struct r_clist_view *r_vws = (struct r_clist_view *) user_data;
	struct f_ctl_real_array *f_r_array = (struct f_ctl_real_array *) g_object_get_data(G_OBJECT(cell), "f_r_array");
	r_tree_value1_edited(path_str, new_text, GTK_TREE_VIEW(r_vws->real_array_tree_view),
						 r_vws->r_clist_gtk);
	update_f_ctl_r_array_by_r_list(r_vws->r_clist_gtk, f_r_array);
/*    write_real_clist(stdout, 0, "value changed", r_vws->r_clist_gtk); */
};

static void add_r_list_items_cb(GtkButton *button, gpointer user_data){
    struct r_clist_view *r_vws = (struct r_clist_view *) user_data;
	struct f_ctl_real_array *f_r_array = (struct f_ctl_real_array *) g_object_get_data(G_OBJECT(button), "f_r_array");
	r_vws->index_bc = add_r_list_items(GTK_TREE_VIEW(r_vws->real_array_tree_view),
									   r_vws->r_clist_gtk);
	reflesh_f_ctl_r_array_by_r_list(r_vws->r_clist_gtk, f_r_array);
/*    write_real_clist(stdout, 0, "columns added", r_vws->r_clist_gtk); */
};

static void delete_r_list_items_cb(GtkButton *button, gpointer user_data){
    struct r_clist_view *r_vws = (struct r_clist_view *) user_data;
	struct f_ctl_real_array *f_r_array = (struct f_ctl_real_array *) g_object_get_data(G_OBJECT(button), "f_r_array");
	delete_r_list_items(GTK_TREE_VIEW(r_vws->real_array_tree_view), r_vws->r_clist_gtk);
	reflesh_f_ctl_r_array_by_r_list(r_vws->r_clist_gtk, f_r_array);
/*    write_real_clist(stdout, 0, "columns deleted", r_vws->r_clist_gtk); */
};

static void init_real_tree_view(struct f_ctl_real_array *f_r_array, struct r_clist_view *r_vws){
	r_vws->real_array_tree_view = gtk_tree_view_new();
	GtkCellRenderer *renderer_spin1 = gtk_cell_renderer_text_new();
	g_object_set_data(G_OBJECT(renderer_spin1), "f_r_array", (gpointer) f_r_array);
	g_signal_connect(G_OBJECT(renderer_spin1), "edited", 
					 G_CALLBACK(r_tree_value1_edited_cb), (gpointer) r_vws);
	
	create_real_tree_view(GTK_TREE_VIEW(r_vws->real_array_tree_view), r_vws->r_clist_gtk, 
                           renderer_spin1);
	
	r_vws->index_bc = append_r_list_from_ctl(r_vws->index_bc,
				&r_vws->r_clist_gtk->r_item_head, GTK_TREE_VIEW(r_vws->real_array_tree_view));
};


GtkWidget * real_array_vbox_w_addbottun(struct f_ctl_real_array *f_r_array,
										struct r_clist_view *r_vws){
	GtkWidget * vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	init_real_tree_view(f_r_array, r_vws);
	
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	g_object_set_data(G_OBJECT(button_add), "f_r_array", (gpointer) f_r_array);
	g_object_set_data(G_OBJECT(button_delete), "f_r_array", (gpointer) f_r_array);
	
	GtkWidget * hbox1 = hbox_with_r_array_checkbox(f_r_array, r_vws);
	GtkWidget *vbox0 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox0), hbox1, FALSE, TRUE, 0);
	
	GtkWidget * hbox0 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	
	char * ctmp = strngcopy_from_f(f_r_array->f_block_name);
	gtk_box_pack_start(GTK_BOX(hbox0), vbox0, FALSE, TRUE, 0);
	GtkWidget *expander = real_list_box_expander(ctmp, r_vws->real_array_tree_view, 
												 button_add, button_delete);
	gtk_box_pack_start(GTK_BOX(hbox0), expander, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox0, FALSE, TRUE, 0);
	
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(add_r_list_items_cb), (gpointer) r_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_r_list_items_cb), (gpointer) r_vws);
	return vbox;
};
