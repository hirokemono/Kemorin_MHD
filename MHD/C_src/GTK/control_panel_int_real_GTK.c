/*
//  control_panel_int_real_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_int_real_GTK.h"

static void copy_f_ctl_ir_array_by_r_list(struct int_real_clist *ir_clist,
										  struct f_ctl_ir_array *f_ir_array)
{
	int i;
	for(i=0;i<f_ir_array->f_num[0];i++){
		f_ir_array->f_ictls[i] = int_real_clist_at_index(i, ir_clist)->i_data;
		f_ir_array->f_rctls[i] = int_real_clist_at_index(i, ir_clist)->r_data;
	}
    return;
}

static void update_f_ctl_ir_array_by_r_list(struct int_real_clist *ir_clist,
											struct f_ctl_ir_array *f_ir_array)
{
/*	c_check_int_real_array(f_ir_array->f_self);*/
	copy_f_ctl_ir_array_by_r_list(ir_clist, f_ir_array);
/*	c_check_int_real_array(f_ir_array->f_self);*/
    return;
}
static void reflesh_f_ctl_ir_array_by_r_list(struct int_real_clist *ir_clist,
											 struct f_ctl_ir_array *f_ir_array)
{
/*	c_check_int_real_array(f_ir_array->f_self);*/
	int num_array = count_int_real_clist(ir_clist);
	reflesh_f_ctl_ir_array(num_array, f_ir_array);
	copy_f_ctl_ir_array_by_r_list(ir_clist, f_ir_array);
/*	c_check_int_real_array(f_ir_array->f_self);*/
    return;
}


void cb_check_ir_array_toggle(GtkWidget *widget, gpointer user_data){
	GtkToggleButton *toggle = GTK_TOGGLE_BUTTON(widget);
	struct ir_clist_view *ir_vws = (struct ir_clist_view *) g_object_get_data(G_OBJECT(widget), "ir_clist_view");
	struct f_ctl_ir_array *f_ir_array = (struct f_ctl_ir_array *) g_object_get_data(G_OBJECT(widget), "f_ir_array");
	
	if(gtk_toggle_button_get_active(toggle) == TRUE){
		reflesh_f_ctl_ir_array(count_int_real_clist(ir_vws->ir_clist_gtk), f_ir_array);
		copy_f_ctl_ir_array_by_r_list(ir_vws->ir_clist_gtk, f_ir_array);
	}else{
		reflesh_f_ctl_ir_array(0, f_ir_array);
	};
	return;
}

static void add_ir_list_items_cb(GtkButton *button, gpointer user_data){
    struct ir_clist_view *ir_vws = (struct ir_clist_view *) user_data;
	struct f_ctl_ir_array *f_ir_array = (struct f_ctl_ir_array *) g_object_get_data(G_OBJECT(button), "f_ir_array");
	ir_vws->index_bc = add_ir_list_items(GTK_TREE_VIEW(ir_vws->ir_tree_view), ir_vws->ir_clist_gtk);
	reflesh_f_ctl_ir_array_by_r_list(ir_vws->ir_clist_gtk, f_ir_array);
};

static void delete_ir_list_items_cb(GtkButton *button, gpointer user_data){
    struct ir_clist_view *ir_vws = (struct ir_clist_view *) user_data;
	struct f_ctl_ir_array *f_ir_array = (struct f_ctl_ir_array *) g_object_get_data(G_OBJECT(button), "f_ir_array");
	delete_ir_list_items(GTK_TREE_VIEW(ir_vws->ir_tree_view), ir_vws->ir_clist_gtk);
	reflesh_f_ctl_ir_array_by_r_list(ir_vws->ir_clist_gtk, f_ir_array);
};


static void ir_tree_value1_edited_cb(GtkCellRenderer *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    struct ir_clist_view *ir_vws = (struct ir_clist_view *) user_data;
	struct f_ctl_ir_array *f_ir_array = (struct f_ctl_ir_array *) g_object_get_data(G_OBJECT(cell), "f_ir_array");
    ir_tree_value1_edited(path_str, new_text, GTK_TREE_VIEW(ir_vws->ir_tree_view), ir_vws->ir_clist_gtk);
	update_f_ctl_ir_array_by_r_list(ir_vws->ir_clist_gtk, f_ir_array);
};

static void ir_tree_value2_edited_cb(GtkCellRenderer *cell, gchar *path_str,
			gchar *new_text, gpointer user_data)
{
    struct ir_clist_view *ir_vws = (struct ir_clist_view *) user_data;
	struct f_ctl_ir_array *f_ir_array = (struct f_ctl_ir_array *) g_object_get_data(G_OBJECT(cell), "f_ir_array");
    ir_tree_value2_edited(path_str, new_text, GTK_TREE_VIEW(ir_vws->ir_tree_view), ir_vws->ir_clist_gtk);
	update_f_ctl_ir_array_by_r_list(ir_vws->ir_clist_gtk, f_ir_array);
};


GtkWidget *hbox_with_ir_array_checkbox(struct f_ctl_ir_array *f_ir_array, struct ir_clist_view *ir_vws){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	GtkWidget *checkbox = gtk_check_button_new();
	g_object_set_data(G_OBJECT(checkbox), "ir_clist_view", (gpointer) ir_vws);
	g_object_set_data(G_OBJECT(checkbox), "f_ir_array", (gpointer) f_ir_array);
	
	if(f_ir_array->f_num[0] == 0){
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), FALSE);
	} else {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), TRUE);
	}
	
	g_signal_connect(G_OBJECT(checkbox), "toggled",
                     G_CALLBACK(cb_check_ir_array_toggle), NULL);
	
	gtk_box_pack_start(GTK_BOX(hbox), checkbox, TRUE, TRUE, 0);
	return hbox;
}


static void init_ir_tree_view(struct f_ctl_ir_array *f_ir_array, struct ir_clist_view *ir_vws){
	GtkCellRenderer *renderer_spin1;
	GtkCellRenderer *renderer_spin2;
	
	ir_vws->ir_tree_view = gtk_tree_view_new();
	renderer_spin1 = gtk_cell_renderer_text_new();
	renderer_spin2 = gtk_cell_renderer_text_new();
	g_object_set_data(G_OBJECT(renderer_spin1), "f_ir_array", (gpointer) f_ir_array);
	g_object_set_data(G_OBJECT(renderer_spin2), "f_ir_array", (gpointer) f_ir_array);
	
	g_signal_connect(G_OBJECT(renderer_spin1), "edited", 
					 G_CALLBACK(ir_tree_value1_edited_cb), (gpointer) ir_vws);
	g_signal_connect(G_OBJECT(renderer_spin2), "edited", 
					 G_CALLBACK(ir_tree_value2_edited_cb), (gpointer) ir_vws);
	
	create_ir_tree_view(GTK_TREE_VIEW(ir_vws->ir_tree_view), ir_vws->ir_clist_gtk, 
                           renderer_spin1, renderer_spin2);
	
	ir_vws->index_bc = append_ir_list_from_ctl(ir_vws->index_bc,
				&ir_vws->ir_clist_gtk->ir_item_head, GTK_TREE_VIEW(ir_vws->ir_tree_view));
};

GtkWidget *  add_ir_list_box_w_addbottun(struct f_ctl_ir_array *f_ir_array, 
										 struct ir_clist_view *ir_vws){
	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	init_ir_tree_view(f_ir_array, ir_vws);
	
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	g_object_set_data(G_OBJECT(button_add), "f_ir_array", (gpointer) f_ir_array);
	g_object_set_data(G_OBJECT(button_delete), "f_ir_array", (gpointer) f_ir_array);
	
	GtkWidget * hbox1 = hbox_with_ir_array_checkbox(f_ir_array, ir_vws);
	GtkWidget *vbox0 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox0), hbox1, FALSE, TRUE, 0);
	
	GtkWidget *hbox0 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	
	char * ctmp = strngcopy_from_f(f_ir_array->f_block_name);
	gtk_box_pack_start(GTK_BOX(hbox0), vbox0, FALSE, TRUE, 0);
	GtkWidget *expander = ir_list_box_expander(ctmp, ir_vws->ir_tree_view, 
											   button_add, button_delete);
	gtk_box_pack_start(GTK_BOX(hbox0), expander, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox0, FALSE, TRUE, 0);
	
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(add_ir_list_items_cb), (gpointer) ir_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_ir_list_items_cb), (gpointer) ir_vws);
	return vbox;
};

