/*
//  control_panel_int_real_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_int_real_GTK.h"

static void copy_f_ctl_ir_array_by_r_list(struct int_real_clist *ir_clist)
{
	int i;
	for(i=0;i<count_int_real_clist(ir_clist);i++){
		c_store_int_real_array(ir_clist->f_self, i,
						   int_real_clist_at_index(i, ir_clist)->i_data,
						   int_real_clist_at_index(i, ir_clist)->r_data);
	}
    return;
}

static void update_f_ctl_ir_array_by_r_list(struct int_real_clist *ir_clist)
{
/*	c_check_int_real_array(ir_clist->f_self);*/
	copy_f_ctl_ir_array_by_r_list(ir_clist);
/*	c_check_int_real_array(ir_clist->f_self);*/
    return;
}
static void reflesh_f_ctl_ir_array_by_r_list(struct int_real_clist *ir_clist)
{
/*	c_check_int_real_array(ir_clist->f_self);*/
	int num_array = count_int_real_clist(ir_clist);
	reflesh_f_ctl_ir_array(num_array, ir_clist);
	copy_f_ctl_ir_array_by_r_list(ir_clist);
/*	c_check_int_real_array(ir_clist->f_self);*/
    return;
}


void cb_check_ir_array_toggle(GtkWidget *widget, gpointer user_data){
	GtkToggleButton *toggle = GTK_TOGGLE_BUTTON(widget);
	struct int_real_clist *ir_clist_gtk = (struct int_real_clist *) g_object_get_data(G_OBJECT(widget), "int_real_clist");
	
	if(gtk_toggle_button_get_active(toggle) == TRUE){
		reflesh_f_ctl_ir_array(count_int_real_clist(ir_clist_gtk), ir_clist_gtk);
		copy_f_ctl_ir_array_by_r_list(ir_clist_gtk);
	}else{
		reflesh_f_ctl_ir_array(0, ir_clist_gtk);
	};
	return;
}

static void add_ir_list_items_cb(GtkButton *button, gpointer user_data){
    GtkWidget *ir_tree_view = GTK_WIDGET(user_data);
	struct int_real_clist *ir_clist_gtk = (struct int_real_clist *) g_object_get_data(G_OBJECT(button), "int_real_clist");
	ir_clist_gtk->index_bc = add_ir_list_items(GTK_TREE_VIEW(ir_tree_view), ir_clist_gtk);
	reflesh_f_ctl_ir_array_by_r_list(ir_clist_gtk);
};

static void delete_ir_list_items_cb(GtkButton *button, gpointer user_data){
    GtkWidget *ir_tree_view = GTK_WIDGET(user_data);
	struct int_real_clist *ir_clist_gtk = (struct int_real_clist *) g_object_get_data(G_OBJECT(button), "int_real_clist");
	delete_ir_list_items(GTK_TREE_VIEW(ir_tree_view), ir_clist_gtk);
	reflesh_f_ctl_ir_array_by_r_list(ir_clist_gtk);
};

static void ir_tree_value1_edited_cb(GtkCellRenderer *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    GtkWidget *ir_tree_view = GTK_WIDGET(user_data);
	struct int_real_clist *ir_clist_gtk = (struct int_real_clist *) g_object_get_data(G_OBJECT(cell), "int_real_clist");
    ir_tree_value1_edited(path_str, new_text, GTK_TREE_VIEW(ir_tree_view), ir_clist_gtk);
	update_f_ctl_ir_array_by_r_list(ir_clist_gtk);
};

static void ir_tree_value2_edited_cb(GtkCellRenderer *cell, gchar *path_str,
			gchar *new_text, gpointer user_data)
{
    GtkWidget *ir_tree_view = GTK_WIDGET(user_data);
	struct int_real_clist *ir_clist_gtk = (struct int_real_clist *) g_object_get_data(G_OBJECT(cell), "int_real_clist");
    ir_tree_value2_edited(path_str, new_text, GTK_TREE_VIEW(ir_tree_view), ir_clist_gtk);
	update_f_ctl_ir_array_by_r_list(ir_clist_gtk);
};


GtkWidget *hbox_with_ir_array_checkbox(struct int_real_clist *ir_clist_gtk){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	GtkWidget *checkbox = gtk_check_button_new();
	
	if(c_int_real_array_num(ir_clist_gtk->f_self) == 0){
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), FALSE);
	} else {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), TRUE);
	}
	
	g_object_set_data(G_OBJECT(checkbox), "int_real_clist", (gpointer) ir_clist_gtk);
	g_signal_connect(G_OBJECT(checkbox), "toggled",
                     G_CALLBACK(cb_check_ir_array_toggle), NULL);
	
	gtk_box_pack_start(GTK_BOX(hbox), checkbox, TRUE, TRUE, 0);
	return hbox;
}


static void init_ir_tree_view(struct int_real_clist *ir_clist_gtk, GtkWidget *ir_tree_view){
	GtkCellRenderer *renderer_value1 = gtk_cell_renderer_text_new();
	GtkCellRenderer *renderer_value2 = gtk_cell_renderer_text_new();
	
	g_object_set_data(G_OBJECT(renderer_value1), "int_real_clist", (gpointer) ir_clist_gtk);
	g_object_set_data(G_OBJECT(renderer_value2), "int_real_clist", (gpointer) ir_clist_gtk);
	
	g_signal_connect(G_OBJECT(renderer_value1), "edited", 
					 G_CALLBACK(ir_tree_value1_edited_cb), (gpointer) ir_tree_view);
	g_signal_connect(G_OBJECT(renderer_value2), "edited", 
					 G_CALLBACK(ir_tree_value2_edited_cb), (gpointer) ir_tree_view);
	
	create_ir_tree_view(GTK_TREE_VIEW(ir_tree_view), ir_clist_gtk, 
                           renderer_value1, renderer_value2);
	
	ir_clist_gtk->index_bc = append_ir_list_from_ctl(ir_clist_gtk->index_bc,
				&ir_clist_gtk->ir_item_head, GTK_TREE_VIEW(ir_tree_view));
};

GtkWidget *  add_ir_list_box_w_addbottun(struct int_real_clist *ir_clist_gtk,
										 GtkWidget *ir_tree_view){
	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	ir_tree_view = gtk_tree_view_new();
	init_ir_tree_view(ir_clist_gtk, ir_tree_view);
	
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	
	GtkWidget * hbox1 = hbox_with_ir_array_checkbox(ir_clist_gtk);
	GtkWidget *vbox0 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox0), hbox1, FALSE, TRUE, 0);
	
	GtkWidget *hbox0 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	
	gtk_box_pack_start(GTK_BOX(hbox0), vbox0, FALSE, TRUE, 0);
	GtkWidget *expander = ir_list_box_expander(ir_clist_gtk->clist_name, ir_tree_view, 
											   button_add, button_delete);
	gtk_box_pack_start(GTK_BOX(hbox0), expander, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox0, FALSE, TRUE, 0);
	
	g_object_set_data(G_OBJECT(button_add), "int_real_clist", (gpointer) ir_clist_gtk);
	g_object_set_data(G_OBJECT(button_delete), "int_real_clist", (gpointer) ir_clist_gtk);
	
	g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(add_ir_list_items_cb), (gpointer) ir_tree_view);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_ir_list_items_cb), (gpointer) ir_tree_view);
	return vbox;
};

