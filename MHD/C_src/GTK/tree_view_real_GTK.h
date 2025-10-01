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
#include "t_ctl_array_single_items_c.h"
#include "tree_views_4_fixed_lists_GTK.h"

/* prototypes */
int append_r_list_from_ctl(int index, struct real_ctl_list *head, 
						   GtkTreeView *r_tree_view);
void r_tree_value1_edited(gchar *path_str, gchar *new_text, GtkTreeView *r_tree_view, 
						  struct real_clist *r_clist);
int add_r_list_items(GtkTreeView *r_tree_view, struct real_clist *r_clist);
void delete_r_list_items(GtkTreeView *r_tree_view, struct real_clist *r_clist);


void create_real_tree_view(GtkTreeView *r_tree_view, struct real_clist *r_clist,
						   GtkCellRenderer *renderer_spin1);


GtkWidget * real_list_box_expander(char *array_name_c, GtkWidget *real_array_tree_view,
								   GtkWidget *button_add, GtkWidget *button_delete);

#endif /* TREE_VIEW_REAL_GTK_H_ */
