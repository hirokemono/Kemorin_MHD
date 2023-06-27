/*
//  control_panel_chara_int_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_chara_int_GTK.h"

static void copy_f_ctl_ci_array_by_r_list(struct chara_int_clist *ci_clist,
										  struct f_ctl_ci_array *f_ci_array)
{
	char *ctmp;
	int i;
	
	for(i=0;i<f_ci_array->f_num[0];i++){
		ctmp = chara_int_clist_at_index(i, ci_clist)->c_tbl;
		f_ci_array->c_charavalue[i] = strngcopy_from_f(ctmp);
	};
	
	int flen = lengthchara_f();
	for(i=0;i<f_ci_array->f_num[0];i++){
		strngcopy(&f_ci_array->f_cctls[i*flen], f_ci_array->c_charavalue[i]);
		load_chara_from_c(&f_ci_array->f_cctls[i*flen]);
		f_ci_array->f_ictls[i] = chara_int_clist_at_index(i, ci_clist)->i_data;
	}
    return;
}

static void update_f_ctl_ci_array_by_r_list(struct chara_int_clist *ci_clist,
											struct f_ctl_ci_array *f_ci_array)
{
/*	c_check_chara_int_array(f_ci_array->f_self);*/
	copy_f_ctl_ci_array_by_r_list(ci_clist, f_ci_array);
/*	c_check_chara_int_array(f_ci_array->f_self);*/
    return;
}
static void reflesh_f_ctl_ci_array_by_r_list(struct chara_int_clist *ci_clist,
											 struct f_ctl_ci_array *f_ci_array)
{
/*	c_check_chara_int_array(f_ci_array->f_self);*/
	int num_array = count_chara_int_clist(ci_clist);
	reflesh_f_ctl_ci_array(num_array, f_ci_array);
	copy_f_ctl_ci_array_by_r_list(ci_clist, f_ci_array);
/*	c_check_chara_int_array(f_ci_array->f_self);*/
    return;
}


void cb_check_ci_array_toggle(GtkWidget *widget, gpointer user_data){
	GtkToggleButton *toggle = GTK_TOGGLE_BUTTON(widget);
	struct ci_clist_view *ci_vws = (struct ci_clist_view *) g_object_get_data(G_OBJECT(widget), "ci_clist_view");
	struct f_ctl_ci_array *f_ci_array = (struct f_ctl_ci_array *) g_object_get_data(G_OBJECT(widget), "f_ci_array");
	
	if(gtk_toggle_button_get_active(toggle) == TRUE){
		reflesh_f_ctl_ci_array(count_chara_int_clist(ci_vws->ci_clist_gtk), f_ci_array);
		copy_f_ctl_ci_array_by_r_list(ci_vws->ci_clist_gtk, f_ci_array);
	}else{
		reflesh_f_ctl_ci_array(0, f_ci_array);
	};
	return;
}

static void add_ci_list_items_cb(GtkButton *button, gpointer user_data){
    struct ci_clist_view *ci_vws = (struct ci_clist_view *) user_data;
	struct f_ctl_ci_array *f_ci_array = (struct f_ctl_ci_array *) g_object_get_data(G_OBJECT(button), "f_ci_array");
	ci_vws->index_bc = add_ci_list_by_bottun_GTK(GTK_TREE_VIEW(ci_vws->ci_tree_view), ci_vws->ci_clist_gtk);
	reflesh_f_ctl_ci_array_by_r_list(ci_vws->ci_clist_gtk, f_ci_array);
};

static void delete_ci_list_items_cb(GtkButton *button, gpointer user_data){
    struct ci_clist_view *ci_vws = (struct ci_clist_view *) user_data;
	struct f_ctl_ci_array *f_ci_array = (struct f_ctl_ci_array *) g_object_get_data(G_OBJECT(button), "f_ci_array");
	delete_ci_list_items_GTK(GTK_TREE_VIEW(ci_vws->ci_tree_view), ci_vws->ci_clist_gtk);
	reflesh_f_ctl_ci_array_by_r_list(ci_vws->ci_clist_gtk, f_ci_array);
};


static void ci_tree_value1_edited_cb(GtkCellRenderer *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    struct ci_clist_view *ci_vws = (struct ci_clist_view *) user_data;
	struct f_ctl_ci_array *f_ci_array = (struct f_ctl_ci_array *) g_object_get_data(G_OBJECT(cell), "f_ci_array");
    ci_tree_name_edited(path_str, new_text, GTK_TREE_VIEW(ci_vws->ci_tree_view), ci_vws->ci_clist_gtk);
	update_f_ctl_ci_array_by_r_list(ci_vws->ci_clist_gtk, f_ci_array);
};

static void ci_tree_value2_edited_cb(GtkCellRenderer *cell, gchar *path_str,
			gchar *new_text, gpointer user_data)
{
    struct ci_clist_view *ci_vws = (struct ci_clist_view *) user_data;
	struct f_ctl_ci_array *f_ci_array = (struct f_ctl_ci_array *) g_object_get_data(G_OBJECT(cell), "f_ci_array");
    ci_tree_value_edited(path_str, new_text, GTK_TREE_VIEW(ci_vws->ci_tree_view), ci_vws->ci_clist_gtk);
	update_f_ctl_ci_array_by_r_list(ci_vws->ci_clist_gtk, f_ci_array);
};


GtkWidget *hbox_with_ci_array_checkbox(struct f_ctl_ci_array *f_ci_array, struct ci_clist_view *ci_vws){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	GtkWidget *checkbox = gtk_check_button_new();
	g_object_set_data(G_OBJECT(checkbox), "ci_clist_view", (gpointer) ci_vws);
	g_object_set_data(G_OBJECT(checkbox), "f_ci_array", (gpointer) f_ci_array);
	
	if(f_ci_array->f_num[0] == 0){
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), FALSE);
	} else {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), TRUE);
	}
	
	g_signal_connect(G_OBJECT(checkbox), "toggled",
                     G_CALLBACK(cb_check_ci_array_toggle), NULL);
	
	gtk_box_pack_start(GTK_BOX(hbox), checkbox, TRUE, TRUE, 0);
	return hbox;
}


static void init_ci_tree_view(struct f_ctl_ci_array *f_ci_array, struct ci_clist_view *ci_vws){
	GtkCellRenderer *renderer_text;
	GtkCellRenderer *renderer_spin;
	
	ci_vws->ci_tree_view = gtk_tree_view_new();
	renderer_text = gtk_cell_renderer_text_new();
	renderer_spin = gtk_cell_renderer_text_new();
	g_object_set_data(G_OBJECT(renderer_text), "f_ci_array", (gpointer) f_ci_array);
	g_object_set_data(G_OBJECT(renderer_spin), "f_ci_array", (gpointer) f_ci_array);
	
	g_signal_connect(G_OBJECT(renderer_text), "edited", 
					 G_CALLBACK(ci_tree_value1_edited_cb), (gpointer) ci_vws);
	g_signal_connect(G_OBJECT(renderer_spin), "edited", 
					 G_CALLBACK(ci_tree_value2_edited_cb), (gpointer) ci_vws);
	
	create_text_int_tree_view(GTK_TREE_VIEW(ci_vws->ci_tree_view), ci_vws->ci_clist_gtk, 
							  renderer_text, renderer_spin);
	
	ci_vws->index_bc = append_ci_list_from_ctl(ci_vws->index_bc,
				&ci_vws->ci_clist_gtk->ci_item_head, GTK_TREE_VIEW(ci_vws->ci_tree_view));
};

GtkWidget *  add_ci_list_box_w_addbottun(struct f_ctl_ci_array *f_ci_array, 
										 struct ci_clist_view *ci_vws){
	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	init_ci_tree_view(f_ci_array, ci_vws);
	
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	g_object_set_data(G_OBJECT(button_add), "f_ci_array", (gpointer) f_ci_array);
	g_object_set_data(G_OBJECT(button_delete), "f_ci_array", (gpointer) f_ci_array);
	
	GtkWidget * hbox1 = hbox_with_ci_array_checkbox(f_ci_array, ci_vws);
	GtkWidget *vbox0 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox0), hbox1, FALSE, TRUE, 0);
	
	GtkWidget *hbox0 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	
	char * ctmp = strngcopy_from_f(f_ci_array->f_block_name);
	gtk_box_pack_start(GTK_BOX(hbox0), vbox0, FALSE, TRUE, 0);
	GtkWidget *expander = ci_list_box_expander(ctmp, ci_vws->ci_tree_view, 
											   button_add, button_delete);
	gtk_box_pack_start(GTK_BOX(hbox0), expander, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox0, FALSE, TRUE, 0);
	
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(add_ci_list_items_cb), (gpointer) ci_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_ci_list_items_cb), (gpointer) ci_vws);
	return vbox;
};

