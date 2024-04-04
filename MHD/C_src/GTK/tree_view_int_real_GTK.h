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

/* prototypes */

int append_ir_list_from_ctl(int index, struct int_real_ctl_list *head, GtkTreeView *ir_tree_view);


void ir_tree_value1_edited(gchar *path_str, gchar *new_text,
			 GtkTreeView *ir_tree_view, struct int_real_clist *ir_clist_gtk);
void ir_tree_value2_edited(gchar *path_str, gchar *new_text,
			 GtkTreeView *ir_tree_view, struct int_real_clist *ir_clist_gtk);

int add_ir_list_items(GtkTreeView *ir_tree_view, struct int_real_clist *ir_clist_gtk);
void delete_ir_list_items(GtkTreeView *ir_tree_view, struct int_real_clist *ir_clist_gtk);

void create_ir_tree_view(GtkTreeView *ir_tree_view, struct int_real_clist *ir_clist_gtk, 
						 GtkCellRenderer *renderer_value1, GtkCellRenderer *renderer_value2);
GtkWidget * ir_list_box_expander(char *array_name_c, GtkWidget *ir_tree_view, 
								 GtkWidget *button_add, GtkWidget *button_delete);

#endif /* TREE_VIEW_INT_REAL_GTK_H_ */
