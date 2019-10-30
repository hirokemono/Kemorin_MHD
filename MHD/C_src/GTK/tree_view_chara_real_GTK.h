//
//  tree_view_chara_real_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/16.
//

#ifndef tree_view_chara_real_GTK_h_
#define tree_view_chara_real_GTK_h_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_chara_real_IO.h"
#include "tree_views_4_fixed_lists_GTK.h"

/* prototypes */


int append_cr_item_to_tree(const int index, const char *c_tbl, const char *c_math, const double r_data, 
			GtkTreeModel *child_model);
int append_cr_list_from_ctl(int index, struct chara_real_ctl_list *head, 
			GtkTreeView *cr_tree_view);

void cr_tree_name_edited(gchar *path_str, gchar *new_text,
			GtkTreeView *cr_tree_view, struct chara_real_clist *cr_clist);
void cr_tree_value_edited(gchar *path_str, gchar *new_text, 
			GtkTreeView *cr_tree_view, struct chara_real_clist *cr_clist);
int add_cr_list_by_bottun_GTK(int index, GtkTreeView *tree_view_to_add, 
                                     struct chara_real_clist *cr_clist);
int add_cr_list_from_combobox_GTK(int index, GtkTreePath *path, GtkTreeModel *tree_model,
			GtkTreeView *tree_view_to_add, struct chara_real_clist *cr_clist);
int add_cr_list_from_combobox_GTK_w_one(int index, GtkTreePath *path, GtkTreeModel *tree_model,
			GtkTreeView *tree_view_to_add, struct chara_real_clist *cr_clist);
int add_cr_list_items_GTK(GtkTreeView *tree_view_to_add,
			struct chara_real_clist *cr_clist);
void delete_cr_list_items_GTK(GtkTreeView *tree_view_to_del,
			struct chara_real_clist *cr_clist);


void create_text_real_tree_view(struct chara_real_clist *cr_clist, GtkTreeView *cr_tree_view,
                                GtkCellRenderer *renderer_text, GtkCellRenderer *renderer_spin);

void add_chara_real_list_box_w_addbottun(GtkTreeView *cr_tree_view, 
			GtkWidget *button_add, GtkWidget *button_delete, 
			GtkWidget *vbox);
void add_chara_real_list_box_w_combobox(GtkTreeView *cr_tree_view, 
			GtkWidget *button_add, GtkWidget *combobox_add, GtkWidget *button_delete, 
			GtkWidget *vbox);

#endif /* tree_view_chara_real_GTK_h_ */
