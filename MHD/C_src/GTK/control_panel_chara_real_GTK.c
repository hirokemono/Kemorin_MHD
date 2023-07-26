/*
//  control_panel_chara_real_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_chara_real_GTK.h"

static void copy_f_ctl_cr_array_by_r_list(struct chara_real_clist *cr_clist)
{
	char *ctmp;
	int i;
	
	for(i=0;i<count_chara_real_clist(cr_clist);i++){
		ctmp = chara_real_clist_at_index(i, cr_clist)->c_tbl;
		c_store_chara_real_array(cr_clist->f_self, i, ctmp,
								 chara_real_clist_at_index(i, cr_clist)->r_data);
	}
    return;
}

static void update_f_ctl_cr_array_by_r_list(struct chara_real_clist *cr_clist)
{
/*	c_check_chara_real_array(cr_clist->f_self);*/
	copy_f_ctl_cr_array_by_r_list(cr_clist);
/*	c_check_chara_real_array(cr_clist->f_self);*/
    return;
}
static void reflesh_f_ctl_cr_array_by_r_list(struct chara_real_clist *cr_clist)
{
/*	c_check_chara_real_array(cr_clist->f_self);*/
	int num_array = count_chara_real_clist(cr_clist);
	reflesh_f_ctl_cr_array(num_array, cr_clist);
	copy_f_ctl_cr_array_by_r_list(cr_clist);
/*	c_check_chara_real_array(cr_clist->f_self);*/
    return;
}


void cb_check_cr_array_toggle(GtkWidget *widget, gpointer user_data){
	GtkToggleButton *toggle = GTK_TOGGLE_BUTTON(widget);
	struct chara_real_clist *cr_clist_gtk = (struct chara_real_clist *) g_object_get_data(G_OBJECT(widget), "cr_clist_gtk");
	if(gtk_toggle_button_get_active(toggle) == TRUE){
		reflesh_f_ctl_cr_array(count_chara_real_clist(cr_clist_gtk), cr_clist_gtk);
		copy_f_ctl_cr_array_by_r_list(cr_clist_gtk);
	}else{
		reflesh_f_ctl_cr_array(0, cr_clist_gtk);
	};
	return;
}

static void add_cr_list_items_cb(GtkButton *button, gpointer user_data){
	GtkWidget *cr_tree_view = GTK_WIDGET(user_data);
	struct chara_real_clist *cr_clist_gtk = (struct chara_real_clist *) g_object_get_data(G_OBJECT(button), "cr_clist_gtk");
	printf("cr_clist_gtk %p %p \n",cr_clist_gtk, cr_tree_view);
	cr_clist_gtk->index_bc = add_cr_list_items_GTK(GTK_TREE_VIEW(cr_tree_view), cr_clist_gtk);
	printf("cr_clist_gtk->index_bc %d \n", cr_clist_gtk->index_bc);
	reflesh_f_ctl_cr_array_by_r_list(cr_clist_gtk);
	printf("cr_clist_gtk %p \n",cr_clist_gtk->f_self);
};

static void delete_cr_list_items_cb(GtkButton *button, gpointer user_data){
    GtkWidget *cr_tree_view = GTK_WIDGET(user_data);
	struct chara_real_clist *cr_clist_gtk = (struct chara_real_clist *) g_object_get_data(G_OBJECT(button), "cr_clist_gtk");
	delete_cr_list_items_GTK(GTK_TREE_VIEW(cr_tree_view), cr_clist_gtk);
	reflesh_f_ctl_cr_array_by_r_list(cr_clist_gtk);
};


static void cr_tree_value1_edited_cb(GtkCellRenderer *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    GtkWidget *cr_tree_view = GTK_WIDGET(user_data);
	struct chara_real_clist *cr_clist_gtk = (struct chara_real_clist *) g_object_get_data(G_OBJECT(cell), "cr_clist_gtk");
    cr_tree_name_edited(path_str, new_text, GTK_TREE_VIEW(cr_tree_view), cr_clist_gtk);
	update_f_ctl_cr_array_by_r_list(cr_clist_gtk);
};

static void cr_tree_value2_edited_cb(GtkCellRenderer *cell, gchar *path_str,
			gchar *new_text, gpointer user_data)
{
    GtkWidget *cr_tree_view = GTK_WIDGET(user_data);
	struct chara_real_clist *cr_clist_gtk = (struct chara_real_clist *) g_object_get_data(G_OBJECT(cell), "cr_clist_gtk");
    cr_tree_value_edited(path_str, new_text, GTK_TREE_VIEW(cr_tree_view), cr_clist_gtk);
	update_f_ctl_cr_array_by_r_list(cr_clist_gtk);
};


GtkWidget *hbox_with_cr_array_checkbox(struct chara_real_clist *cr_clist_gtk){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	GtkWidget *checkbox = gtk_check_button_new();
	g_object_set_data(G_OBJECT(checkbox), "cr_clist_gtk", (gpointer) cr_clist_gtk);
	
	if(count_chara_real_clist(cr_clist_gtk) == 0){
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), FALSE);
	} else {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), TRUE);
	}
	
	g_signal_connect(G_OBJECT(checkbox), "toggled",
                     G_CALLBACK(cb_check_cr_array_toggle), NULL);
	
	gtk_box_pack_start(GTK_BOX(hbox), checkbox, TRUE, TRUE, 0);
	return hbox;
}


static void init_cr_tree_view(struct chara_real_clist *cr_clist_gtk, GtkWidget *cr_tree_view){
	GtkCellRenderer *renderer_text;
	GtkCellRenderer *renderer_value;
	
	renderer_text =  gtk_cell_renderer_text_new();
	renderer_value = gtk_cell_renderer_text_new();
	g_object_set_data(G_OBJECT(renderer_text), "cr_clist_gtk", (gpointer) cr_clist_gtk);
	g_object_set_data(G_OBJECT(renderer_value), "cr_clist_gtk", (gpointer) cr_clist_gtk);
	
	g_signal_connect(G_OBJECT(renderer_text), "edited", 
					 G_CALLBACK(cr_tree_value1_edited_cb), (gpointer) cr_tree_view);
	g_signal_connect(G_OBJECT(renderer_value), "edited", 
					 G_CALLBACK(cr_tree_value2_edited_cb), (gpointer) cr_tree_view);
	
	create_text_real_tree_view(cr_clist_gtk, GTK_TREE_VIEW(cr_tree_view), 
							  renderer_text, renderer_value);
	
	cr_clist_gtk->index_bc = append_cr_list_from_ctl(cr_clist_gtk->index_bc, &cr_clist_gtk->cr_item_head, 
                                                     GTK_TREE_VIEW(cr_tree_view));
};

GtkWidget * add_cr_list_box_w_addbottun(struct chara_real_clist *cr_clist_gtk,
                                        GtkWidget *cr_tree_view){
	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	cr_tree_view = gtk_tree_view_new();
	init_cr_tree_view(cr_clist_gtk, cr_tree_view);
	
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	g_object_set_data(G_OBJECT(button_add), "cr_clist_gtk", (gpointer) cr_clist_gtk);
	g_object_set_data(G_OBJECT(button_delete), "cr_clist_gtk", (gpointer) cr_clist_gtk);
	
	GtkWidget * hbox1 = hbox_with_cr_array_checkbox(cr_clist_gtk);
	GtkWidget *vbox0 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox0), hbox1, FALSE, TRUE, 0);
	
	GtkWidget *hbox0 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	
	gtk_box_pack_start(GTK_BOX(hbox0), vbox0, FALSE, TRUE, 0);
	GtkWidget *expander = cr_list_box_expander(cr_clist_gtk->clist_name, GTK_TREE_VIEW(cr_tree_view), 
											   button_add, button_delete);
	gtk_box_pack_start(GTK_BOX(hbox0), expander, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox0, FALSE, TRUE, 0);
	
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(add_cr_list_items_cb), (gpointer) cr_tree_view);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_cr_list_items_cb), (gpointer) cr_tree_view);
	return vbox;
};

