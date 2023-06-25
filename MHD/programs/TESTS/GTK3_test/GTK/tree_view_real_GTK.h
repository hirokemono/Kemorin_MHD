/*
//  tree_view_real_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef TREE_VIEW_REAL_GTK_H_
#define TREE_VIEW_REAL_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_real_IO.h"
#include "tree_views_4_fixed_lists_GTK.h"

struct r_clist_view{
    int index_bc;
    GtkWidget *real_array_tree_view;
    
    struct real_clist *r_clist_gtk;
};

/* prototypes */

void init_r_clist_views(struct real_clist *r_clist, struct r_clist_view *cmap_vws);
int append_r_item_to_tree(int index, double r1_data, GtkTreeModel *child_model);
int append_r_list_from_ctl(int index, struct real_ctl_list *head, 
			GtkTreeView *r_tree_view);


void r_tree_value1_edited(gchar *path_str, gchar *new_text,
			 GtkTreeView *r_tree_view, struct real_clist *r_clist);

int add_r_list_items(GtkTreeView *r_tree_view, struct real_clist *r_clist);
void delete_r_list_items(GtkTreeView *r_tree_view, struct real_clist *r_clist);

void create_real_tree_view(GtkTreeView *r_tree_view, struct real_clist *r_clist, 
                            GtkCellRenderer *renderer_spin1);
void add_real_list_box(GtkTreeView *r_tree_view, struct real_clist *r_clist, 
			GtkWidget *button_add, GtkWidget *button_delete, GtkWidget *vbox);

void r_tree_value1_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data);
void add_r_list_items_cb(GtkButton *button, gpointer user_data);
void delete_r_list_items_cb(GtkButton *button, gpointer user_data);


void init_real_tree_view(struct r_clist_view *r_vws);
GtkWidget * real_array_vbox_w_addbottun(struct r_clist_view *r_vws);


#endif /* TREE_VIEW_REAL_GTK_H_ */
