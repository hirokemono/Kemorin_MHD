/*
//  calypso_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2019/10/29.
*/

#include "calypso_GTK.h"

static void set_last_field_to_label_CB(GtkTreeSelection *selection, gpointer user_data)
{
    GtkTreeModel *model;
    GList *cur;
    gchar *row_string;
    GtkTreeIter iter;

    GList *list = gtk_tree_selection_get_selected_rows(selection, &model);

    for (cur = g_list_first(list); cur != NULL; cur = g_list_next(cur)) {
        if (gtk_tree_model_get_iter(model, &iter, (GtkTreePath *)cur->data) == TRUE) {
            gtk_tree_model_get(model, &iter, COLUMN_FIELD_NAME, &row_string, -1);
        }
        gtk_tree_path_free((GtkTreePath *)cur->data);
    }
    g_list_free(list);
}


static void column_clicked_CB(GtkTreeViewColumn *column, gpointer user_data)
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
}

static void column_clicked_to_sort_CB(GtkTreeViewColumn *column, gpointer user_data)
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


void block_changed_signal(GObject *instance)
{
	GList *list;
	GList *cur;
	gulong handler_id;
	GtkTreeSelection *selection;

	list = g_object_get_data(G_OBJECT(instance), "selection_list");
	for (cur = g_list_first(list); cur != NULL; cur = g_list_next(cur)) {
		selection = cur->data;
		handler_id = GPOINTER_TO_UINT(g_object_get_data(G_OBJECT(selection), "changed_handler_id"));
		g_signal_handler_block(G_OBJECT(selection), handler_id);
	}
}

void unblock_changed_signal(GObject *instance)
{
	GList *list;
	GList *cur;
	gulong handler_id;
	GtkTreeSelection *selection;
	
	list = g_object_get_data(G_OBJECT(instance), "selection_list");
	for (cur = g_list_first(list); cur != NULL; cur = g_list_next(cur)) {
		selection = cur->data;
		handler_id = GPOINTER_TO_UINT(g_object_get_data(G_OBJECT(selection), "changed_handler_id"));
		g_signal_handler_unblock(G_OBJECT(selection), handler_id);

		/* Apply changes during signal's blocking */
		set_last_field_to_label_CB(selection, NULL);
	}
}


void add_sorting_signal_w_label(GtkTreeView *tree_view, GtkWidget *hbox){
    GtkTreeSelection *selection;
	GtkTreeModel *model;
    GtkTreeModel *child_model;
    gulong changed_handler_id;
    GtkWidget *label;
    GList *list;
	
    /*
     * selectionにchangedシグナルハンドラを登録する。
     * 後で同じchild_modelを使用しているselectionのchangedシグナルをブロック出来るように
     * child_modelにselectionのリストを、selectionにシグナルハンドラIDを登録する。
     * changedハンドラ内で使用するlabelも同様に登録しておく。
     */
    label = gtk_label_new("");
    gtk_box_pack_end(GTK_BOX(hbox), label, TRUE, TRUE, 0);
	
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view));
    changed_handler_id = g_signal_connect(G_OBJECT(selection), "changed",
                                          G_CALLBACK(set_last_field_to_label_CB), NULL);
    g_object_set_data(G_OBJECT(selection), "changed_handler_id", GUINT_TO_POINTER(changed_handler_id));
    g_object_set_data(G_OBJECT(selection), "label", label);
    
    model = gtk_tree_view_get_model(GTK_TREE_VIEW(tree_view));
    child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    list = g_object_get_data(G_OBJECT(child_model), "selection_list");
    list = g_list_append(list, selection);
    g_object_set_data(G_OBJECT(child_model), "selection_list", list);
	return;
}


GtkTreeViewColumn * create_each_column_no_sort(GtkWidget *tree_view,
			const char *label, int column_index)
{
    GtkTreeViewColumn *column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), column);
    gtk_tree_view_column_set_title(column, label);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(column_index));
	g_signal_connect(G_OBJECT(column), "clicked", G_CALLBACK(column_clicked_CB),
				(gpointer) tree_view);
	return column;
};

GtkTreeViewColumn * create_each_field_column(GtkWidget *tree_view,
			const char *label, int column_index)
{
    GtkTreeViewColumn *column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), column);
    gtk_tree_view_column_set_title(column, label);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(column_index));
	g_signal_connect(G_OBJECT(column), "clicked", G_CALLBACK(column_clicked_to_sort_CB),
				(gpointer) tree_view);
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

