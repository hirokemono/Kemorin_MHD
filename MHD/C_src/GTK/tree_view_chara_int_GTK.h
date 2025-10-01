/*
//  tree_view_chara_int_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/21.
*/

#ifndef tree_view_chara_int_GTK_h_
#define tree_view_chara_int_GTK_h_

#include "t_control_chara_int_IO.h"
#include "tree_views_4_fixed_lists_GTK.h"

/* prototypes */

int append_ci_item_to_tree(const int index, const char *c_tbl, const int i_data, GtkTreeModel *child_model);
int append_ci_list_from_ctl(int index, struct chara_int_ctl_list *head, 
			GtkTreeView *ci_tree_view);
int clear_ci_tree_view(GtkTreeView *ci_tree_view);

void ci_tree_name_edited(gchar *path_str, gchar *new_text,
			GtkTreeView *ci_tree_view, struct chara_int_clist *ci_clist);
void ci_tree_value_edited(gchar *path_str, gchar *new_text, 
			GtkTreeView *ci_tree_view, struct chara_int_clist *ci_clist);
int add_ci_list_by_bottun_GTK(int index, GtkTreeView *tree_view_to_add, 
                                     struct chara_int_clist *ci_clist);
int add_ci_list_from_combobox_GTK(int index, GtkTreePath *path, GtkTreeModel *tree_model,
			GtkTreeView *tree_view_to_add, struct chara_int_clist *ci_clist);
int add_ci_list_from_combobox_GTK_w_one(int index, GtkTreePath *path, GtkTreeModel *tree_model,
			GtkTreeView *tree_view_to_add, struct chara_int_clist *ci_clist);
int add_ci_list_items_GTK(GtkTreeView *tree_view_to_add,
			struct chara_int_clist *ci_clist);
void delete_ci_list_items_GTK(GtkTreeView *tree_view_to_del,
			struct chara_int_clist *ci_clist);


void create_text_int_tree_view(struct chara_int_clist *ci_clist, GtkTreeView *ci_tree_view,
							   GtkCellRenderer *renderer_text, GtkCellRenderer *renderer_value);

GtkWidget *ci_list_box_expander(char * array_name_c, GtkTreeView *ci_tree_view, 
								GtkWidget *button_add, GtkWidget *button_delete);
void add_chara_int_list_box_w_combobox(GtkTreeView *ci_tree_view, 
			GtkWidget *button_add, GtkWidget *combobox_add, GtkWidget *button_delete, 
			GtkWidget *vbox);


#endif /* tree_view_chara_int_GTK_h_ */
