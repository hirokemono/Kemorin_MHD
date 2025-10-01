/*
//  control_panel_chara_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_chara_GTK.h"

static void copy_f_ctl_c_array_by_r_list(struct chara_clist *c_clist)
{
	int i;
	for(i=0;i<count_chara_clist(c_clist);i++){
        struct chara_ctl_item *tmp_item = chara_clist_at_index(i, c_clist);
		c_store_chara_array(c_clist->f_self, i, tmp_item->c_tbl);
	}
    return;
}

static void update_f_ctl_c_array_by_r_list(struct chara_clist *c_clist)
{
	if(c_clist->f_self == NULL) return;
	copy_f_ctl_c_array_by_r_list(c_clist);
    return;
}
static void reflesh_f_ctl_c_array_by_r_list(struct chara_clist *c_clist)
{
	if(c_clist->f_self == NULL) return;
	int num_array = count_chara_clist(c_clist);
	reflesh_f_ctl_chara_array(num_array, c_clist);
	copy_f_ctl_c_array_by_r_list(c_clist);
    return;
}


void cb_check_c_array_toggle(GtkWidget *widget, gpointer user_data){
	GtkToggleButton *toggle = GTK_TOGGLE_BUTTON(widget);
	struct chara_clist *c_clist_gtk = (struct chara_clist *) g_object_get_data(G_OBJECT(widget), "c_clist_gtk");
	if(gtk_toggle_button_get_active(toggle) == TRUE){
		reflesh_f_ctl_chara_array(count_chara_clist(c_clist_gtk), c_clist_gtk);
		copy_f_ctl_c_array_by_r_list(c_clist_gtk);
	}else{
		reflesh_f_ctl_chara_array(0, c_clist_gtk);
	};
	return;
}

static void add_c_list_items_cb(GtkButton *button, gpointer user_data){
	GtkWidget *c_tree_view = GTK_WIDGET(user_data);
	struct chara_clist *c_clist_gtk = (struct chara_clist *) g_object_get_data(G_OBJECT(button), "c_clist_gtk");
	c_clist_gtk->index_bc = add_c_list_items_GTK(GTK_TREE_VIEW(c_tree_view), c_clist_gtk);
	reflesh_f_ctl_c_array_by_r_list(c_clist_gtk);
};

static void delete_c_list_items_cb(GtkButton *button, gpointer user_data){
    GtkWidget *c_tree_view = GTK_WIDGET(user_data);
	struct chara_clist *c_clist_gtk = (struct chara_clist *) g_object_get_data(G_OBJECT(button), "c_clist_gtk");
	delete_c_list_items_GTK(GTK_TREE_VIEW(c_tree_view), c_clist_gtk);
	reflesh_f_ctl_c_array_by_r_list(c_clist_gtk);
};


static void chara_tree_value1_edited_cb(GtkCellRenderer *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    GtkWidget *c_tree_view = GTK_WIDGET(user_data);
	struct chara_clist *c_clist_gtk = (struct chara_clist *) g_object_get_data(G_OBJECT(cell), "c_clist_gtk");
    c_tree_name_edited(path_str, new_text, GTK_TREE_VIEW(c_tree_view), c_clist_gtk);
	update_f_ctl_c_array_by_r_list(c_clist_gtk);
};

GtkWidget *hbox_with_c_array_checkbox(struct chara_clist *c_clist_gtk){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	GtkWidget *checkbox = gtk_check_button_new();
	g_object_set_data(G_OBJECT(checkbox), "c_clist_gtk", (gpointer) c_clist_gtk);
	
	if(count_chara_clist(c_clist_gtk) == 0){
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), FALSE);
	} else {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), TRUE);
	}
	
	g_signal_connect(G_OBJECT(checkbox), "toggled",
                     G_CALLBACK(cb_check_c_array_toggle), NULL);
	
	gtk_box_pack_start(GTK_BOX(hbox), checkbox, TRUE, TRUE, 0);
	return hbox;
}


static void init_c_tree_view(struct chara_clist *c_clist_gtk, GtkWidget *c_tree_view){
	GtkCellRenderer *renderer_text;
	
	renderer_text =  gtk_cell_renderer_text_new();
	g_object_set_data(G_OBJECT(renderer_text), "c_clist_gtk", (gpointer) c_clist_gtk);
	
	g_signal_connect(G_OBJECT(renderer_text), "edited", 
					 G_CALLBACK(chara_tree_value1_edited_cb), (gpointer) c_tree_view);
	
	create_text_tree_view(GTK_TREE_VIEW(c_tree_view), renderer_text);
	
	c_clist_gtk->index_bc = append_c_list_from_ctl_w_index(c_clist_gtk->index_bc, c_clist_gtk,
                                                           GTK_TREE_VIEW(c_tree_view));
};

GtkWidget * add_c_list_box_w_addbottun(struct chara_clist *c_clist_gtk,
									   GtkWidget *c_tree_view){
	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	c_tree_view = gtk_tree_view_new();
	init_c_tree_view(c_clist_gtk, c_tree_view);
	
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	g_object_set_data(G_OBJECT(button_add), "c_clist_gtk", (gpointer) c_clist_gtk);
	g_object_set_data(G_OBJECT(button_delete), "c_clist_gtk", (gpointer) c_clist_gtk);
	
	GtkWidget * hbox1 = hbox_with_c_array_checkbox(c_clist_gtk);
	GtkWidget *vbox0 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox0), hbox1, FALSE, TRUE, 0);
	
	GtkWidget *hbox0 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(hbox0), vbox0, FALSE, TRUE, 0);
	
	GtkWidget *expander = chara_list_box_expander(c_clist_gtk->clist_name, GTK_TREE_VIEW(c_tree_view), 
												  button_add, button_delete);
	gtk_box_pack_start(GTK_BOX(hbox0), expander, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox0, FALSE, TRUE, 0);
	
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(add_c_list_items_cb), (gpointer) c_tree_view);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_c_list_items_cb), (gpointer) c_tree_view);
	return vbox;
};

