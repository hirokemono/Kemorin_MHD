/*
//  tree_view_chara_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/21.
*/

#ifndef tree_view_chara_GTK_h_
#define tree_view_chara_GTK_h_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_chara_IO.h"
#include "tree_views_4_fixed_lists_GTK.h"

/* prototypes */


int append_c_item_to_tree(int index, const char *c_tbl, GtkTreeModel *child_model);
int append_c_list_from_ctl(int index, struct chara_ctl_list *head, 
			GtkTreeView *c_tree_view);
int append_c_list_from_array(int index, int num, char **c_tbl, 
                             GtkTreeView *c_tree_view);

void c_tree_name_edited(gchar *path_str, gchar *new_text,
			GtkTreeView *c_tree_view, struct chara_clist *c_clist);
int add_c_list_by_bottun_GTK(int index, GtkTreeView *tree_view_to_add, 
                                     struct chara_clist *c_clist);
int add_c_list_from_combobox_GTK(int index, GtkTreePath *path, GtkTreeModel *tree_model,
			GtkTreeView *tree_view_to_add, struct chara_clist *c_clist);
int add_c_list_from_combobox_GTK_w_one(int index, GtkTreePath *path, GtkTreeModel *tree_model,
			GtkTreeView *tree_view_to_add, struct chara_clist *c_clist);
int add_c_list_items_GTK(GtkTreeView *tree_view_to_add,
			struct chara_clist *c_clist);
void delete_c_list_items_GTK(GtkTreeView *tree_view_to_del,
			struct chara_clist *c_clist);


void create_text_tree_view(GtkTreeView *c_tree_view,
			GtkCellRenderer *renderer_text, GtkCellRenderer *renderer_spin);

void add_chara_list_box_w_addbottun(GtkTreeView *c_tree_view, 
			GtkWidget *button_add, GtkWidget *button_delete, 
			GtkWidget *vbox);
void add_chara_list_box_w_combobox(GtkTreeView *c_tree_view, 
			GtkWidget *button_add, GtkWidget *combobox_add, GtkWidget *button_delete, 
			GtkWidget *vbox);

#endif /* tree_view_chara_GTK_h_ */
