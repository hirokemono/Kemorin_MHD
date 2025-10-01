/*
//  control_panel_int_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_int_GTK.h"

static void copy_f_ctl_int_array_by_r_list(struct int_clist *i_clist)
{
	int i;
	for(i=0;i<count_int_clist(i_clist);i++){
		c_store_int_array(i_clist->f_self, i,
						  int_clist_at_index(i, i_clist)->i_data);
	}
    return;
}

static void update_f_ctl_i_array_by_r_list(struct int_clist *i_clist)
{
/*	c_check_int2_array(i_clist->f_self);*/
	copy_f_ctl_int_array_by_r_list(i_clist);
/*	c_check_int2_array(i_clist->f_self);*/
    return;
}
static void reflesh_f_ctl_int_array_by_r_list(struct int_clist *i_clist)
{
/*	c_check_int2_array(i_clist->f_self);*/
	int num_array = count_int_clist(i_clist);
	reflesh_f_ctl_int_array(num_array, i_clist);
	copy_f_ctl_int_array_by_r_list(i_clist);
/*	c_check_int2_array(i_clist->f_self);*/
    return;
}


void cb_check_int_array_toggle(GtkWidget *widget, gpointer user_data){
	GtkToggleButton *toggle = GTK_TOGGLE_BUTTON(widget);
	struct int_clist *i_clist_gtk = (struct int_clist *) g_object_get_data(G_OBJECT(widget), "int_clist");
	
	if(gtk_toggle_button_get_active(toggle) == TRUE){
		int num_array = count_int_clist(i_clist_gtk);
		reflesh_f_ctl_int_array(num_array, i_clist_gtk);
		copy_f_ctl_int_array_by_r_list(i_clist_gtk);
	}else{
		reflesh_f_ctl_int_array(0, i_clist_gtk);
	};
	return;
}

static void add_int_list_items_cb(GtkButton *button, gpointer user_data){
    GtkWidget *int_tree_view = GTK_WIDGET(user_data);
	struct int_clist *i_clist_gtk = (struct int_clist *) g_object_get_data(G_OBJECT(button), "int_clist");
	i_clist_gtk->index_bc = add_int_list_items(GTK_TREE_VIEW(int_tree_view), i_clist_gtk);
	reflesh_f_ctl_int_array_by_r_list(i_clist_gtk);
};

static void delete_int_list_items_cb(GtkButton *button, gpointer user_data){
    GtkWidget *int_tree_view = GTK_WIDGET(user_data);
	struct int_clist *i_clist_gtk = (struct int_clist *) g_object_get_data(G_OBJECT(button), "int_clist");
	delete_int_list_items(GTK_TREE_VIEW(int_tree_view), i_clist_gtk);
	reflesh_f_ctl_int_array_by_r_list(i_clist_gtk);
};


static void int_tree_value1_edited_cb(GtkCellRenderer *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    GtkWidget *int_tree_view = GTK_WIDGET(user_data);
	struct int_clist *i_clist_gtk = (struct int_clist *) g_object_get_data(G_OBJECT(cell), "int_clist");
    int_tree_value1_edited(path_str, new_text, GTK_TREE_VIEW(int_tree_view), i_clist_gtk);
	update_f_ctl_i_array_by_r_list(i_clist_gtk);
};

static GtkWidget *hbox_with_int_array_checkbox(struct int_clist *i_clist_gtk){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	GtkWidget *checkbox = gtk_check_button_new();
	g_object_set_data(G_OBJECT(checkbox), "int_clist", (gpointer) i_clist_gtk);
	
	if(c_int_array_num(i_clist_gtk->f_self) == 0){
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), FALSE);
	} else {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), TRUE);
	}
	
	g_signal_connect(G_OBJECT(checkbox), "toggled",
                     G_CALLBACK(cb_check_int_array_toggle), NULL);
	
	gtk_box_pack_start(GTK_BOX(hbox), checkbox, TRUE, TRUE, 0);
	return hbox;
}


	static void init_int_tree_view(struct int_clist *i_clist_gtk, 
								   GtkWidget *int_tree_view)
			{
	GtkCellRenderer *renderer_value1 = gtk_cell_renderer_text_new();
	
	g_object_set_data(G_OBJECT(renderer_value1),    "int_clist", (gpointer) i_clist_gtk);
	
	g_signal_connect(G_OBJECT(renderer_value1), "edited", 
					 G_CALLBACK(int_tree_value1_edited_cb), (gpointer) int_tree_view);
	
	create_int_tree_view(GTK_TREE_VIEW(int_tree_view), i_clist_gtk,
                           renderer_value1);
	
	i_clist_gtk->index_bc = append_int_list_from_ctl(i_clist_gtk->index_bc,
													 &i_clist_gtk->i_item_head,
													 GTK_TREE_VIEW(int_tree_view));
};

GtkWidget *  add_int_list_box_w_addbottun(struct int_clist *i_clist_gtk, GtkWidget *int_tree_view){
	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	int_tree_view = gtk_tree_view_new();
	init_int_tree_view(i_clist_gtk, int_tree_view);
	
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	
	GtkWidget * hbox1 = hbox_with_int_array_checkbox(i_clist_gtk);
	GtkWidget *vbox0 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox0), hbox1, FALSE, TRUE, 0);
	
	GtkWidget *hbox0 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	
	gtk_box_pack_start(GTK_BOX(hbox0), vbox0, FALSE, TRUE, 0);
	GtkWidget *expander = int_list_box_expander(i_clist_gtk->clist_name, int_tree_view,
												button_add, button_delete);
	gtk_box_pack_start(GTK_BOX(hbox0), expander, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox0, FALSE, TRUE, 0);
	
	g_object_set_data(G_OBJECT(button_add),    "int_clist", (gpointer) i_clist_gtk);
	g_object_set_data(G_OBJECT(button_delete), "int_clist", (gpointer) i_clist_gtk);
	
	g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(add_int_list_items_cb), (gpointer) int_tree_view);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_int_list_items_cb), (gpointer) int_tree_view);
	return vbox;
};

