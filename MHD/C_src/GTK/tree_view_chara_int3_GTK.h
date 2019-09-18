/*
//  tree_view_chara_int3_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/21.
*/

#ifndef tree_view_chara_int3_GTK_h_
#define tree_view_chara_int3_GTK_h_

#include "t_control_chara_int3_IO.h"
#include "tree_views_4_fixed_lists_GTK.h"

/* prototypes */

int append_ci3_item_to_tree(int index, char *c_tbl, int i1_data, int i2_data, int i3_data, 
			GtkTreeModel *child_model);
int append_ci3_list_from_ctl(int index, struct chara_int3_ctl_list *head, 
			GtkTreeView *ci3_tree_view);

void ci3_tree_name_edited(gchar *path_str, gchar *new_text,
			GtkTreeView *ci3_tree_view, struct chara_int3_clist *ci3_clist);
void ci3_tree_value1_edited(gchar *path_str, gchar *new_text, 
			GtkTreeView *ci3_tree_view, struct chara_int3_clist *ci3_clist);
void ci3_tree_value2_edited(gchar *path_str, gchar *new_text, 
			GtkTreeView *ci3_tree_view, struct chara_int3_clist *ci3_clist);
void ci3_tree_value3_edited(gchar *path_str, gchar *new_text, 
			GtkTreeView *ci3_tree_view, struct chara_int3_clist *ci3_clist);
int add_ci3_list_by_bottun_GTK(int index, GtkTreeView *tree_view_to_add, 
                                     struct chara_int3_clist *ci3_clist);
int add_ci3_list_from_combobox_GTK(int index, GtkTreePath *path, GtkTreeModel *tree_model,
			GtkTreeView *tree_view_to_add, struct chara_int3_clist *ci3_clist);
int add_ci3_list_from_combobox_GTK_w_one(int index, GtkTreePath *path, GtkTreeModel *tree_model,
			GtkTreeView *tree_view_to_add, struct chara_int3_clist *ci3_clist);
int add_ci3_list_items_GTK(GtkTreeView *tree_view_to_add,
			struct chara_int3_clist *ci3_clist);
void delete_ci3_list_items_GTK(GtkTreeView *tree_view_to_del,
			struct chara_int3_clist *ci3_clist);


void create_text_int3_tree_view(struct chara_int3_clist *ci3_clist, GtkTreeView *ci3_tree_view,
			GtkCellRenderer *renderer_text, GtkCellRenderer *renderer_spin);

void add_chara_int3_list_box_w_addbottun(GtkTreeView *ci3_tree_view, 
			GtkWidget *button_add, GtkWidget *button_delete, 
			GtkWidget *vbox);
void add_chara_int3_list_box_w_combobox(GtkTreeView *ci3_tree_view, 
			GtkWidget *button_add, GtkWidget *combobox_add, GtkWidget *button_delete, 
			GtkWidget *vbox);


#endif /* tree_view_chara_int_GTK_h_ */
