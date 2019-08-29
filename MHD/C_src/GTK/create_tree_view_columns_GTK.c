/*
//  create_tree_view_columns_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/14.
*/

#include "create_tree_view_columns_GTK.h"

static void column_clicked(GtkTreeViewColumn *column, gpointer user_data)
{
	GtkTreeView *tree_view = GTK_TREE_VIEW(user_data);
	GtkTreeModel *model;
	gint column_id;
	gint cur_id;
	GtkSortType order;
	GtkTreeViewColumn *cur_column;
	
	if (gtk_widget_is_focus(GTK_WIDGET(tree_view)) == FALSE) {
		gtk_widget_grab_focus(GTK_WIDGET(tree_view));
	}
	
	column_id = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(column), "column_id"));
	model = gtk_tree_view_get_model(tree_view);
	
	/* 現在のソート列と同じときは昇順／降順を反転する、違うときはクリックした列で昇順ソートする */
	if (gtk_tree_sortable_get_sort_column_id(GTK_TREE_SORTABLE(model), &cur_id, &order) == TRUE) {
		if (cur_id == column_id) {
			order = (order == GTK_SORT_ASCENDING) ? GTK_SORT_DESCENDING : GTK_SORT_ASCENDING;
		} else {
			order = GTK_SORT_ASCENDING;
		}
		cur_column = gtk_tree_view_get_column(tree_view, cur_id);
		gtk_tree_view_column_set_sort_indicator(cur_column, FALSE);
	} else {
		order = GTK_SORT_ASCENDING;
	}
	gtk_tree_view_column_set_sort_order(column, order);
	gtk_tree_view_column_set_sort_indicator(column, TRUE);
	gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), column_id, order);
}

GtkTreeViewColumn * create_each_field_column(GtkWidget *tree_view,
			const char *label, int column_index)
{
    GtkTreeViewColumn *column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), column);
    gtk_tree_view_column_set_title(column, label);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(column_index));
	g_signal_connect(G_OBJECT(column), "clicked", G_CALLBACK(column_clicked), (gpointer) tree_view);
	return column;
};

GtkCellRenderer * create_each_text_renderer(GtkTreeViewColumn *column,
			int iwidth, int column_index){
	GtkCellRenderer *renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_attributes(column, renderer, "text", column_index, NULL);
	g_object_set(renderer, "width", (gint) iwidth, NULL);
	return renderer;
};

GtkCellRenderer * create_each_toggle_renderer(GtkTreeViewColumn *column, 
			int iwidth, int column_index){
	GtkCellRenderer *renderer = gtk_cell_renderer_toggle_new();
    gtk_tree_view_column_pack_start(column, renderer, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer, "active", column_index, NULL);
    g_object_set(renderer, "width", (gint) iwidth, NULL);
	return renderer;
};

