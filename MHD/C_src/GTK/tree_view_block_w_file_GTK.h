/*
//  tree_view_block_w_file_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/21.
*/

#ifndef TREE_VIEW_BLOCK_W_FILE_GTK_H_
#define TREE_VIEW_BLOCK_W_FILE_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_c_lists.h"
#include "t_control_c_lists_w_file.h"

/* prototypes */


int append_void_file_item_to_tree(int index, const char *c_tbl,
								  GtkTreeModel *child_model);
int append_void_file_list_from_ctl(int index, struct void_file_ctl_list *head, 
								   GtkTreeView *v_tree_view);

int add_void_file_list_by_bottun_GTK(int index, void *void_in, GtkTreeView *tree_view_to_add, 
									 struct void_file_clist *vf_clist);
int add_void_file_list_items_GTK(GtkTreeView *tree_view_to_add, 
								 void *(*append_ctl_block_F)(int idx, char *block_name, void *f_parent), 
								 void *(*init_ctl_block_F)(int idx, void *f_parent), 
								 void *(*init_ctl_block_file_F)(int idx, void *f_parent), 
								 void *(*dealloc_ctl_block_F)(void *f_block), 
								 struct void_file_clist *vf_clist);
void delete_void_file_list_items_GTK(GtkTreeView *tree_view_to_del,
									 void *(*delete_ctl_block_F)(int idx, void *f_parent), 
									 void *(*init_ctl_block_F)(int idx, void *f_parent), 
									 void *(*init_ctl_block_file_F)(int idx, void *f_parent), 
									 void *(*dealloc_ctl_block_F)(void *f_block), 
									 struct void_file_clist *vf_clist);


void create_file_block_tree_view(GtkTreeView *v_tree_view, GtkCellRenderer *renderer_text);

GtkWidget * add_file_block_box_w_addbottun(struct void_file_clist *void_file_clist, GtkWidget *v_tree_view, 
										   GtkWidget *button_add, GtkWidget *button_delete,
										   GtkWidget *vbox_out);

#endif /* TREE_VIEW_BLOCK_W_FILE_GTK_H_ */
