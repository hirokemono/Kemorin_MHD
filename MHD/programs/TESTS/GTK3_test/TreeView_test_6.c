#include <stdlib.h>
#include <gtk/gtk.h>

#include "t_ctl_data_4_fields_c.h"
#include "t_SGS_MHD_control_c.h"

struct all_field_ctl_c **all_fld_list;
struct SGS_MHD_control_c *mhd_ctl;
char file_name[LENGTHBUF] = "/Users/matsui/work/C_test/control_MHD";
char buf[LENGTHBUF];      /* character buffer for reading line */

static GtkWidget *main_window = NULL;

enum {
	COLUMN_FIELD_INDEX = 0,
	COLUMN_FIELD_NAME,
	COLUMN_FIELD_MATH,
	COLUMN_VIZ_FLAG,
	COLUMN_MONITOR_FLAG,
	COLUMN_NUM_COMP,
};


static void sum_selected_rows(GtkTreeSelection *selection, gpointer user_data)
{
	GtkLabel *label;
	GtkTreeModel *model;
	GList *list;
	GList *cur;
	gint sum = 0;
	gchar buf[64];

	label = g_object_get_data(G_OBJECT(selection), "label");

	list = gtk_tree_selection_get_selected_rows(selection, &model);
	if (list == NULL) {
		gtk_label_set_text(label, "");
		return;
	}

	for (cur = g_list_first(list); cur != NULL; cur = g_list_next(cur)) {
		GtkTreeIter iter;
		gint num;
		if (gtk_tree_model_get_iter(model, &iter, (GtkTreePath *)cur->data) == TRUE) {
			gtk_tree_model_get(model, &iter, COLUMN_FIELD_INDEX, &num, -1);
			sum += num;
		}
		gtk_tree_path_free((GtkTreePath *)cur->data);
	}
	g_list_free(list);

	snprintf(buf, sizeof(buf), "%d", sum);
	gtk_label_set_text(label, buf);
}

/* Append new data at the end of list */
static void init_model_data(GtkTreeView *tree_view)
{
	static gint serial_num = 0;
	GtkTreeModel *model;
	GtkTreeModel *child_model;
	GtkTreeIter iter;
	int i;
	model = gtk_tree_view_get_model(tree_view);
	child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
	
	for(i=0;i<NUM_FIELD;i++){
		if(all_fld_list[i]->iflag_use > 0){
			
			gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
			gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
					   COLUMN_FIELD_INDEX, i,
					   COLUMN_FIELD_NAME, all_fld_list[i]->field_name,
					   COLUMN_VIZ_FLAG, all_fld_list[i]->iflag_viz,
					   COLUMN_MONITOR_FLAG, all_fld_list[i]->iflag_monitor,
					   COLUMN_NUM_COMP, all_fld_list[i]->num_comp,
						-1);
			
		};
	}
}

static void append_model_data(GtkButton *button, gpointer user_data)
{
	static gint serial_num = 0;
	GtkTreeView *tree_view = GTK_TREE_VIEW(user_data);
	GtkTreeModel *model;
	GtkTreeModel *child_model;
	GtkTreeIter iter;

	model = gtk_tree_view_get_model(tree_view);
	child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));

	gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
	gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
					   COLUMN_FIELD_INDEX, ++serial_num,
					   COLUMN_FIELD_NAME, all_fld_list[serial_num]->field_name,
					   COLUMN_VIZ_FLAG, rand() & 0x0000FFFF,
					   -1);
}

static void block_changed_signal(GObject *instance)
{
	GList *list;
	GList *cur;

	list = g_object_get_data(G_OBJECT(instance), "selection_list");
	for (cur = g_list_first(list); cur != NULL; cur = g_list_next(cur)) {
		GtkTreeSelection *selection = cur->data;
		gulong handler_id;
		handler_id = GPOINTER_TO_UINT(g_object_get_data(G_OBJECT(selection), "changed_handler_id"));
		g_signal_handler_block(G_OBJECT(selection), handler_id);
	}
}

static void unblock_changed_signal(GObject *instance)
{
	GList *list;
	GList *cur;

	list = g_object_get_data(G_OBJECT(instance), "selection_list");
	for (cur = g_list_first(list); cur != NULL; cur = g_list_next(cur)) {
		GtkTreeSelection *selection = cur->data;
		gulong handler_id;
		handler_id = GPOINTER_TO_UINT(g_object_get_data(G_OBJECT(selection), "changed_handler_id"));
		g_signal_handler_unblock(G_OBJECT(selection), handler_id);

		/* changedシグナルをブロックしていた間の変更を反映させる */
		sum_selected_rows(selection, NULL);
	}
}

static void remove_model_data(GtkButton *button, gpointer user_data)
{
	GtkTreeView *tree_view = GTK_TREE_VIEW(user_data);
	GtkTreeModel *model;
	GtkTreeModel *child_model;
	GtkTreeSelection *selection;
	GList *list;
	GList *reference_list;
	GList *cur;

	/* 選択されている行のパスを取得する */
	/* パスはツリーモデルソートのもの */
	model = gtk_tree_view_get_model(tree_view);
	selection = gtk_tree_view_get_selection(tree_view);
	list = gtk_tree_selection_get_selected_rows(selection, NULL);
	child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));

	/* 最初にパスからリファレンスを作成する */
	/* データの削除を行なうと取得済みのパスが(大抵の場合)無効になる */
	reference_list = NULL;
	for (cur = g_list_first(list); cur != NULL; cur = g_list_next(cur)) {
		GtkTreePath *child_path;
		GtkTreeRowReference *child_reference;
		/* ツリーモデルソートのパスをツリーモデルのパスに変換する */
		child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), (GtkTreePath *)cur->data);

		child_reference = gtk_tree_row_reference_new(child_model, child_path);
		reference_list = g_list_append(reference_list, child_reference);

		gtk_tree_path_free(child_path);
		gtk_tree_path_free((GtkTreePath *)cur->data);
	}
	g_list_free(list);

	/* GtkTreeSelectionのchangedシグナルを一時的にブロックする */
	block_changed_signal(G_OBJECT(child_model));

	/* リファレンスをパスに戻して削除 */
	for (cur = g_list_first(reference_list); cur != NULL; cur = g_list_next(cur)) {
		GtkTreePath *tree_path;
		GtkTreeIter iter;
		tree_path = gtk_tree_row_reference_get_path((GtkTreeRowReference *)cur->data);
		gtk_tree_model_get_iter(child_model, &iter, tree_path);

		/* 削除 */
		gtk_list_store_remove(GTK_LIST_STORE(child_model), &iter);

		gtk_tree_path_free(tree_path);
		gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
	}
	g_list_free(reference_list);

	/* changedシグナルのブロックを解除する */
	unblock_changed_signal(G_OBJECT(child_model));
}

static GtkWidget *create_window(GtkWidget *tree_view, gint window_id)
{
	GtkWidget *window;
	gchar *title;
	GtkWidget *vbox;
	GtkWidget *hbox;
	GtkWidget *button;
	GtkWidget *label;
	GtkWidget *scrolled_window;
	GtkTreeSelection *selection;
	GtkTreeModel *model;
	GtkTreeModel *child_model;
	gulong changed_handler_id;
	GList *list;

	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	title = g_strdup_printf("GtkTreeModelSort #%d", window_id);
	gtk_window_set_title(GTK_WINDOW(window), title);
	g_free(title);

	vbox = gtk_vbox_new(FALSE, 0);

	hbox = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

	/* データ追加ボタン */
	button = gtk_button_new_from_stock(GTK_STOCK_ADD);
	gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(append_model_data), tree_view);

	/* データ削除ボタン */
	button = gtk_button_new_from_stock(GTK_STOCK_REMOVE);
	gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(remove_model_data), tree_view);

	/* ラベル */
	label = gtk_label_new("");
	gtk_box_pack_end(GTK_BOX(hbox), label, TRUE, TRUE, 0);

	scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scrolled_window, 400, 300);
	gtk_container_add(GTK_CONTAINER(scrolled_window), tree_view);

	gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, TRUE, TRUE, 0);

	gtk_container_add(GTK_CONTAINER(window), vbox);

	/*
	 * selectionにchangedシグナルハンドラを登録する。
	 * 後で同じchild_modelを使用しているselectionのchangedシグナルをブロック出来るように
	 * child_modelにselectionのリストを、selectionにシグナルハンドラIDを登録する。
	 * changedハンドラ内で使用するlabelも同様に登録しておく。
	 */
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view));
	changed_handler_id = g_signal_connect(G_OBJECT(selection), "changed", G_CALLBACK(sum_selected_rows), NULL);
	g_object_set_data(G_OBJECT(selection), "changed_handler_id", GUINT_TO_POINTER(changed_handler_id));
	g_object_set_data(G_OBJECT(selection), "label", label);

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(tree_view));
	child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
	list = g_object_get_data(G_OBJECT(child_model), "selection_list");
	list = g_list_append(list, selection);
	g_object_set_data(G_OBJECT(child_model), "selection_list", list);

	gtk_widget_show_all(window);

	return window;
}

static void column_clicked(GtkTreeViewColumn *column, gpointer user_data)
{
	GtkTreeView *tree_view = GTK_TREE_VIEW(user_data);
	GtkTreeModel *model;
	gint column_id;
	gint cur_id;
	GtkSortType order;
	GtkTreeViewColumn *cur_column;
	
	GtkTreeViewColumn *button;
	
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

static void create_tree_view_window(GtkButton *button, gpointer user_data)
{
	static gint window_id = 0;
	GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);
	GtkTreeModel *model;
	GtkWidget *tree_view;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	GtkTreeSelection *selection;
	
	int i;
	
	/* ソート用のモデルを作成してツリービューにセットする */
	tree_view = gtk_tree_view_new();
	model = gtk_tree_model_sort_new_with_model(child_model);
	gtk_tree_view_set_model(GTK_TREE_VIEW(tree_view), model);

	/* First raw */
	column = gtk_tree_view_column_new();
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), column);
	gtk_tree_view_column_set_title(column, "Index");
	renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_FIELD_INDEX, NULL);
	g_object_set(renderer, "width", (gint)60, NULL);
	gtk_tree_view_column_set_resizable(column, TRUE);
	gtk_tree_view_column_set_clickable(column, TRUE);
	g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_INDEX));
	g_signal_connect(G_OBJECT(column), "clicked", G_CALLBACK(column_clicked), tree_view);

	/* Second row */
	column = gtk_tree_view_column_new();
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), column);
	gtk_tree_view_column_set_title(column, "Field name");
	renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_FIELD_NAME, NULL);
	g_object_set(renderer, "width", (gint)150, NULL);
	gtk_tree_view_column_set_resizable(column, TRUE);
	gtk_tree_view_column_set_clickable(column, TRUE);
	g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_NAME));
	g_signal_connect(G_OBJECT(column), "clicked", G_CALLBACK(column_clicked), tree_view);

	/* Third row */
	column = gtk_tree_view_column_new();
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), column);
	gtk_tree_view_column_set_title(column, "Component");
	renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_NUM_COMP, NULL);
	g_object_set(renderer, "width", (gint)60, NULL);
	gtk_tree_view_column_set_resizable(column, TRUE);
	gtk_tree_view_column_set_clickable(column, TRUE);
	g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_NUM_COMP));
	g_signal_connect(G_OBJECT(column), "clicked", G_CALLBACK(column_clicked), tree_view);

	/* Forth row */
	column = gtk_tree_view_column_new();
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), column);
	gtk_tree_view_column_set_title(column, "Field output");
	renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_VIZ_FLAG, NULL);
	g_object_set(renderer, "width", (gint)60, NULL);
	gtk_tree_view_column_set_resizable(column, TRUE);
	gtk_tree_view_column_set_clickable(column, TRUE);
	g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_VIZ_FLAG));
	g_signal_connect(G_OBJECT(column), "clicked", G_CALLBACK(column_clicked), tree_view);

	/* Fifth row */
	column = gtk_tree_view_column_new();
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), column);
	gtk_tree_view_column_set_title(column, "Monitor output");
	renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_MONITOR_FLAG, NULL);
	g_object_set(renderer, "width", (gint)60, NULL);
	gtk_tree_view_column_set_resizable(column, TRUE);
	gtk_tree_view_column_set_clickable(column, TRUE);
	g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_MONITOR_FLAG));
	g_signal_connect(G_OBJECT(column), "clicked", G_CALLBACK(column_clicked), tree_view);

	/* 選択モード */
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view));
	gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);

	/* 1行毎に背景色を変更 */
	gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(tree_view), TRUE);

	/* ソート */
	column = gtk_tree_view_get_column(GTK_TREE_VIEW(tree_view), COLUMN_FIELD_INDEX);
	gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
	gtk_tree_view_column_set_sort_indicator(column, TRUE);
	gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_INDEX, GTK_SORT_ASCENDING);
	
	init_model_data(tree_view);
	
	/* ウィンドウ作成 */
	create_window(tree_view, ++window_id);
}

int main(int argc, char **argv)
{
	GtkListStore *store;
	GtkWidget *hbox;
	GtkWidget *button;

	srand((unsigned)time(NULL));

    all_fld_list = (struct all_field_ctl_c **) malloc(NUM_FIELD * sizeof(struct all_field_ctl_c *));
    alloc_all_field_ctl_c(all_fld_list);
	
    mhd_ctl = alloc_SGS_MHD_control_c();
	read_SGS_MHD_control_file_c(file_name, buf, mhd_ctl);
    load_field_from_ctl(&mhd_ctl->model_ctl->fld_ctl->field_list, all_fld_list);
	
	
	gtk_init(&argc, &argv);

	/* 空のリストストアを作成する */
	store = gtk_list_store_new(6, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING,
				G_TYPE_INT, G_TYPE_INT, G_TYPE_INT);
	g_object_set_data(G_OBJECT(store), "selection_list", NULL);

	main_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(main_window), "GtkTreeModelSort");
	g_signal_connect(G_OBJECT(main_window), "destroy", G_CALLBACK(gtk_main_quit), NULL);

	hbox = gtk_hbox_new(TRUE, 10);

	button = gtk_button_new_with_label("Create Window");
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(create_tree_view_window), store);
	gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);

	gtk_container_add(GTK_CONTAINER(main_window), hbox);
	gtk_container_set_border_width(GTK_CONTAINER(main_window), 10);
	gtk_widget_show_all(main_window);

	gtk_main();
	return 0;
}
