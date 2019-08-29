/*
//  create_tree_view_columns_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/14.
*/


#ifndef create_tree_view_columns_GTK_
#define create_tree_view_columns_GTK_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gtk/gtk.h>

GtkTreeViewColumn * create_each_field_column(GtkWidget *tree_view,
			const char *label, int column_index);
GtkCellRenderer * create_each_text_renderer(GtkTreeViewColumn *column,
			int iwidth, int column_index);

GtkCellRenderer * create_each_toggle_renderer(GtkTreeViewColumn *column, 
			int iwidth, int column_index);

#endif /* create_tree_view_columns_GTK_ */
