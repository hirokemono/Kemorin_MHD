#include <gtk/gtk.h>

enum {
	COLUMN_NUMBER = 0
};

void callback_button_clicked(GtkButton *button, gpointer user_data)
{
	GtkTreeView *tree_view = GTK_TREE_VIEW(user_data);
	GtkTreeSelection *selection;
	GtkTreeModel *filter;
	GtkTreeModel *model;
	GList *rows;
	GList *cur;

	g_print("callback_button_clicked()\n");

	filter = gtk_tree_view_get_model(tree_view);
	model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(filter));

	/* 選択されている値を削除する */
	selection = gtk_tree_view_get_selection(tree_view);
	rows = gtk_tree_selection_get_selected_rows(selection, NULL);

	/* 最初にfilterのpathをmodelのpathに変換し、さらにmodelのpathからrow referenceを作成する */
	for (cur = g_list_first(rows); cur != NULL; cur = g_list_next(cur)) {
		GtkTreePath *tree_path;
		GtkTreeRowReference *reference;

		tree_path = gtk_tree_model_filter_convert_path_to_child_path(GTK_TREE_MODEL_FILTER(filter), (GtkTreePath *)cur->data);
		reference = gtk_tree_row_reference_new(model, tree_path);

		gtk_tree_path_free(tree_path);
		gtk_tree_path_free((GtkTreePath *)cur->data);

		cur->data = reference;
	}

	/* 次にrow referenceからpathに戻し、iterを取得して削除する */
	for (cur = g_list_first(rows); cur != NULL; cur = g_list_next(cur)) {
		GtkTreePath *tree_path;
		GtkTreeIter iter;

		tree_path = gtk_tree_row_reference_get_path((GtkTreeRowReference *)cur->data);
		gtk_tree_model_get_iter(model, &iter, tree_path);
		gtk_list_store_remove(GTK_LIST_STORE(model), &iter);

		gtk_tree_path_free(tree_path);
		gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
		cur->data = NULL;
	}
	g_list_free(rows);
}

void callback_selection_changed(GtkTreeSelection *selection, gpointer user_data)
{
	GtkLabel *label = GTK_LABEL(user_data);
	GtkTreeView *tree_view;
	GtkTreeModel *filter;
	GtkTreeModel *model;
	GList *rows;
	GList *cur;
	gint value;
	gint sum = 0;
	gchar label_text[20];

	g_print("callback_selection_changed()\n");

	tree_view = gtk_tree_selection_get_tree_view(selection);
	filter = gtk_tree_view_get_model(tree_view);
	model = gtk_tree_model_filter_get_model(GTK_TREE_MODEL_FILTER(filter));

	/* 選択されている値の合計をラベルに表示する */
	rows = gtk_tree_selection_get_selected_rows(selection, NULL);

	for (cur = g_list_first(rows); cur != NULL; cur = g_list_next(cur)) {
#ifdef FIX_CONVERT_TO_CHILD
		/* 最初にfilterのtree pathからmodelのtree pathへ変換し、次にmodelのiterを取得する */
		GtkTreePath *tree_path;
		GtkTreeIter iter;

		tree_path = gtk_tree_model_filter_convert_path_to_child_path(GTK_TREE_MODEL_FILTER(filter), (GtkTreePath *)cur->data);
		if (tree_path == NULL) {
			g_print("*** modelのpathを取得できません ***\n");
			continue;
		}
		if (gtk_tree_model_get_iter(model, &iter, tree_path) == FALSE) {
			g_print("*** modelのiterを取得できません ***\n");
			continue;
		}
		gtk_tree_model_get(model, &iter, COLUMN_NUMBER, &value, -1);
		g_print("正常な場合: %d\n", value);
		sum += value;

		gtk_tree_path_free(tree_path);
#else
		/* 最初にfilterのiterを取得し、次にmodelのiterへ変換する */
		GtkTreeIter filter_iter;
		GtkTreeIter iter;

		if (gtk_tree_model_get_iter(filter, &filter_iter, (GtkTreePath *)cur->data) == FALSE) {
			g_print("*** filterのiterを取得できません ***\n");
			continue;
		}
		gtk_tree_model_filter_convert_iter_to_child_iter(GTK_TREE_MODEL_FILTER(filter), &iter, &filter_iter);
		gtk_tree_model_get(model, &iter, COLUMN_NUMBER, &value, -1);	/* 連続した行を削除するとここで落ちます */
		g_print("エラーになる場合: %d\n", value);
		sum += value;
#endif
	}
	g_list_free(rows);

	g_snprintf(label_text, sizeof(label_text), "合計 = %d", sum);
	gtk_label_set_text(label, label_text);
}

gboolean filter_visible_func(GtkTreeModel *model, GtkTreeIter *iter, gpointer data)
{
	return TRUE;			/* すべて表示する */
}

GtkListStore *create_list_store(void)
{
	GtkListStore *list_store;
	GtkTreeIter iter;
	gint i;

	/* 列は1つだけ */
	list_store = gtk_list_store_new(1, G_TYPE_INT);

	/* 1〜30の数値をセットする */
	for (i = 0; i < 30; i++) {
		gtk_list_store_append(list_store, &iter);
		gtk_list_store_set(list_store, &iter, COLUMN_NUMBER, i, -1);
	}

	return list_store;
}

GtkWidget *create_tree_view(void)
{
	GtkWidget *tree_view;
	GtkTreeModel *filter;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	GtkTreeSelection *selection;
	GtkListStore *list_store;

	tree_view = gtk_tree_view_new();

	/* 列作成 */
	column = gtk_tree_view_column_new();
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), column);
	gtk_tree_view_column_set_title(column, "値");
	renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_NUMBER, NULL);

	/* 選択モード設定 */
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view));
	gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);

	/* データ作成 */
	list_store = create_list_store();

	/* フィルタ設定 */
	filter = gtk_tree_model_filter_new(GTK_TREE_MODEL(list_store), NULL);
	gtk_tree_model_filter_set_visible_func(GTK_TREE_MODEL_FILTER(filter), filter_visible_func, NULL, NULL);

	/* データセット */
	gtk_tree_view_set_model(GTK_TREE_VIEW(tree_view), filter);
	g_object_unref(G_OBJECT(list_store));

	return tree_view;
}

int main(int argc, char **argv)
{
	GtkWidget *main_window;
	GtkWidget *vbox;
	GtkWidget *hbox;
	GtkWidget *scrolled_window;
	GtkWidget *label;
	GtkWidget *button;
	GtkWidget *store;
	GtkWidget *tree_view;
	GtkWidget *tree_column;

	gtk_init(&argc, &argv);

	/* 画面作成 */
	main_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(main_window), "GtkTreeSelection - change");

	vbox = gtk_vbox_new(FALSE, FALSE);
	gtk_container_add(GTK_CONTAINER(main_window), vbox);

	hbox = gtk_hbox_new(FALSE, FALSE);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

	label = gtk_label_new("");
	gtk_box_pack_start(GTK_BOX(hbox), label, TRUE, TRUE, 0);

	button = gtk_button_new_with_label("削除");
	gtk_box_pack_end(GTK_BOX(hbox), button, FALSE, FALSE, 0);

	scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scrolled_window, 200, 400);
	gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, TRUE, TRUE, 0);

	tree_view = create_tree_view();
	gtk_container_add(GTK_CONTAINER(scrolled_window), tree_view);

	/* イベントハンドラ設定 */
	g_signal_connect(G_OBJECT(main_window), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(callback_button_clicked), tree_view);
	g_signal_connect(G_OBJECT(gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view))), "changed", G_CALLBACK(callback_selection_changed), label);


	gtk_widget_show_all(main_window);

	gtk_main();
	return 0;
}
