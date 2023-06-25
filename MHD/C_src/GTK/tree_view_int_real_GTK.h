/*
//  tree_view_int_real_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef TREE_VIEW_INT_REAL_GTK_H_
#define TREE_VIEW_INT_REAL_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_int_real_IO.h"
#include "tree_views_4_fixed_lists_GTK.h"

struct ir_clist_view{
    int index_bc;
    GtkWidget *ir_tree_view;
    
    struct int_real_clist *ir_clist_gtk;
};

/* prototypes */

void init_ir_clist_views(struct int_real_clist *ir_clist_gtk, struct ir_clist_view *ir_vws);
int append_ir_item_to_tree(int index, double r1_data, double r2_data, 
			GtkTreeModel *child_model);
int append_ir_list_from_ctl(int index, struct int_real_ctl_list *head, GtkTreeView *ir_tree_view);


void ir_tree_value1_edited(gchar *path_str, gchar *new_text,
			 GtkTreeView *ir_tree_view, struct int_real_clist *ir_clist_gtk);
void ir_tree_value2_edited(gchar *path_str, gchar *new_text,
			 GtkTreeView *ir_tree_view, struct int_real_clist *ir_clist_gtk);

/*static void column_clicked(GtkTreeViewColumn *column, gpointer user_data);*/

int add_ir_list_items(GtkTreeView *ir_tree_view, struct int_real_clist *ir_clist_gtk);
void delete_ir_list_items(GtkTreeView *ir_tree_view, struct int_real_clist *ir_clist_gtk);

void create_ir_tree_view(GtkTreeView *ir_tree_view, struct int_real_clist *ir_clist_gtk, 
                            GtkCellRenderer *renderer_spin1, GtkCellRenderer *renderer_spin2);
void add_ir_list_box(GtkTreeView *ir_tree_view, struct int_real_clist *ir_clist_gtk, 
			GtkWidget *button_add, GtkWidget *button_delete, GtkWidget *vbox);

void ir_tree_value1_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data);
void ir_tree_value2_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data);
void add_ir_list_items_cb(GtkButton *button, gpointer user_data);
void delete_ir_list_items_cb(GtkButton *button, gpointer user_data);


void init_ir_tree_view(struct ir_clist_view *ir_vws);
void add_ir_list_box_w_addbottun(struct ir_clist_view *ir_vws, GtkWidget *vbox);


#endif /* TREE_VIEW_INT_REAL_GTK_H_ */
