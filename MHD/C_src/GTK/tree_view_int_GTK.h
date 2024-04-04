/*
//  tree_view_int_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef TREE_VIEW_INT_GTK_H_
#define TREE_VIEW_INT_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_int_IO.h"
#include "tree_views_4_fixed_lists_GTK.h"

/* prototypes */

int append_int_list_from_ctl(int index, struct int_ctl_list *head,
							 GtkTreeView *int_tree_view);


void int_tree_value1_edited(gchar *path_str, gchar *new_text,
							GtkTreeView *int_tree_view, struct int_clist *i_clist_gtk);

int add_int_list_items(GtkTreeView *int_tree_view, struct int_clist *i_clist_gtk);
void delete_int_list_items(GtkTreeView *int_tree_view, struct int_clist *i_clist_gtk);

void create_int_tree_view(GtkTreeView *int_tree_view, struct int_clist *i_clist_gtk, 
						  GtkCellRenderer *renderer_value1);
GtkWidget * int_list_box_expander(char *array_name_c, GtkWidget *int_tree_view, 
								  GtkWidget *button_add, GtkWidget *button_delete);

#endif /* TREE_VIEW_INT_GTK_H_ */
