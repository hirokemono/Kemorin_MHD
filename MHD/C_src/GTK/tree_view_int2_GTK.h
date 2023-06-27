/*
//  tree_view_int2_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef TREE_VIEW_INT2_GTK_H_
#define TREE_VIEW_INT2_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_int2_IO.h"
#include "tree_views_4_fixed_lists_GTK.h"

/* prototypes */

int append_i2_list_from_ctl(int index, struct int2_ctl_list *head, GtkTreeView *i2_tree_view);


void i2_tree_value1_edited(gchar *path_str, gchar *new_text,
			 GtkTreeView *i2_tree_view, struct int2_clist *i2_clist_gtk);
void i2_tree_value2_edited(gchar *path_str, gchar *new_text,
			 GtkTreeView *i2_tree_view, struct int2_clist *i2_clist_gtk);

int add_i2_list_items(GtkTreeView *i2_tree_view, struct int2_clist *i2_clist_gtk);
void delete_i2_list_items(GtkTreeView *i2_tree_view, struct int2_clist *i2_clist_gtk);

void create_i2_tree_view(GtkTreeView *i2_tree_view, struct int2_clist *i2_clist_gtk, 
						 GtkCellRenderer *renderer_value1, GtkCellRenderer *renderer_value2);
GtkWidget * i2_list_box_expander(char *array_name_c, GtkWidget *i2_tree_view, 
								 GtkWidget *button_add, GtkWidget *button_delete);

#endif /* TREE_VIEW_INT2_GTK_H_ */
