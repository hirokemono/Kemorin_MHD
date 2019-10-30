/*
//  tree_view_real2_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef tree_view_real2_GTK_h_
#define tree_view_real2_GTK_h_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_real2_IO.h"
#include "tree_views_4_fixed_lists_GTK.h"

struct r2_clist_view{
    int index_bc;
    GtkWidget *tree_view;
    
    struct real2_clist *r2_clist_gtk;
};

/* prototypes */

void init_r2_clist_views(struct real2_clist *r2_clist, struct r2_clist_view *cmap_vws);
int append_r2_item_to_tree(int index, double r1_data, double r2_data, 
			GtkTreeModel *child_model);
int append_r2_list_from_ctl(int index, struct real2_ctl_list *head, 
			GtkTreeView *r2_tree_view);


void r2_tree_value1_edited(gchar *path_str, gchar *new_text,
			 GtkTreeView *r2_tree_view, struct real2_clist *r2_clist);
void r2_tree_value2_edited(gchar *path_str, gchar *new_text,
			 GtkTreeView *r2_tree_view, struct real2_clist *r2_clist);

/*static void column_clicked(GtkTreeViewColumn *column, gpointer user_data);*/

int add_r2_list_items(GtkTreeView *r2_tree_view, struct real2_clist *r2_clist);
void delete_r2_list_items(GtkTreeView *r2_tree_view, struct real2_clist *r2_clist);

void create_real2_tree_view(GtkTreeView *r2_tree_view, struct real2_clist *r2_clist, 
                            GtkCellRenderer *renderer_spin1, GtkCellRenderer *renderer_spin2);
void add_real2_list_box(GtkTreeView *r2_tree_view, struct real2_clist *r2_clist, 
			GtkWidget *button_add, GtkWidget *button_delete, GtkWidget *vbox);

void r2_tree_value1_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data);
void r2_tree_value2_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data);
void add_r2_list_items_cb(GtkButton *button, gpointer user_data);
void delete_r2_list_items_cb(GtkButton *button, gpointer user_data);


void init_real2_tree_view(struct r2_clist_view *r2_vws);
void add_real2_list_box_w_addbottun(struct r2_clist_view *r2_vws, GtkWidget *vbox);


#endif /* tree_view_real2_GTK_h_ */
