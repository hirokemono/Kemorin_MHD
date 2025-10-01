/*
//  tree_view_block_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/21.
*/

#ifndef TREE_VIEW_BLOCK_GTK_H_
#define TREE_VIEW_BLOCK_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_c_lists.h"
#include "tree_views_4_fixed_lists_GTK.h"

/* prototypes */


int append_void_item_to_tree(int index, const char *c_tbl, GtkTreeModel *child_model);
int append_void_list_from_ctl(int index, struct void_ctl_list *head, 
							  GtkTreeView *v_tree_view);

int add_void_list_by_bottun_GTK(int index, void *void_in, GtkTreeView *tree_view_to_add, 
								struct void_clist *v_clist);
int add_void_list_items_GTK(GtkTreeView *tree_view_to_add, 
							void *(*append_ctl_block_F)(int idx, char *block_name, void *f_parent), 
							void *(*init_block_item)(int idx, void *f_parent, void *void_in_gtk),
							void *(*dealloc_block_item)(void *f_block),
                            void *void_in_gtk, struct void_clist *v_clist);
void delete_void_list_items_GTK(GtkTreeView *tree_view_to_del,
								void *(*delete_ctl_block_F)(int idx, void *f_parent), 
								void *(*init_block_item)(int idx, void *f_parent, void *void_in_gtk),
								void *(*dealloc_block_item)(void *f_block), 
                                void *void_in_gtk, struct void_clist *v_clist);


void create_block_tree_view(GtkTreeView *v_tree_view, GtkCellRenderer *renderer_text);

GtkWidget * add_block_list_box_w_addbottun(struct void_clist *v_clist_gtk, GtkWidget *v_tree_view, 
										   GtkWidget *button_add, GtkWidget *button_delete,
										   GtkWidget *vbox_out);

#endif /* TREE_VIEW_BLOCK_GTK_H_ */
