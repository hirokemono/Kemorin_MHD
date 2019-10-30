/*
//  calypso_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2019/10/29.
*/

#ifndef CALYPSO_GTK_h_
#define CALYPSO_GTK_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef DEPENDENCY_CHECK
  #include <gtk/gtk.h>
#endif

#define COLUMN_FIELD_INDEX    0
#define COLUMN_FIELD_NAME     1
#define COLUMN_FIELD_MATH     2
#define COLUMN_FORTH   3
#define COLUMN_FIFTH   4
#define COLUMN_NUM_COMP       5
#define COLUMN_QUADRATURE     6


/* prototype */

void block_changed_signal(GObject *instance);
void unblock_changed_signal(GObject *instance);
void add_sorting_signal_w_label(GtkTreeView *tree_view, GtkWidget *hbox);


GtkTreeViewColumn * create_each_column_no_sort(GtkWidget *tree_view,
			const char *label, int column_index);
GtkTreeViewColumn * create_each_field_column(GtkWidget *tree_view,
			const char *label, int column_index);
GtkCellRenderer * create_each_text_renderer(GtkTreeViewColumn *column,
			int iwidth, int column_index);

GtkCellRenderer * create_each_toggle_renderer(GtkTreeViewColumn *column, 
			int iwidth, int column_index);

#endif /* CALYPSO_GTK_h_ */
