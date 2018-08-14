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
	return;
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
					   COLUMN_VIZ_FLAG, (gboolean) all_fld_list[i]->iflag_viz,
					   COLUMN_MONITOR_FLAG, (gboolean) all_fld_list[i]->iflag_monitor,
					   COLUMN_NUM_COMP, all_fld_list[i]->num_comp,
						-1);
			
		};
	}
}

static void cb_close_window(GtkButton *button, gpointer user_data){
	GtkWidget *window = (GtkWidget *) user_data;
	gtk_widget_destroy(window);
	gtk_widget_show_all(main_window);
};


static void append_model_data(GtkButton *menubutton, gpointer user_data)
{
	printf("Add \n");
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
    
    gchar *row_string;
    int index_field;

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
        gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
        gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &row_string, -1);
        /*
        printf("To be deleted: %d, %s: %s\n", index_field, row_string,
               all_fld_list[index_field]->field_name);
*/
		/* DElete */
		gtk_list_store_remove(GTK_LIST_STORE(child_model), &iter);
        delete_field_in_ctl(all_fld_list[index_field], &mhd_ctl->model_ctl->fld_ctl->field_list);

		gtk_tree_path_free(tree_path);
		gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
	}
	g_list_free(reference_list);

	/* changedシグナルのブロックを解除する */
	unblock_changed_signal(G_OBJECT(child_model));
    /*
    check_field_ctl_list(&mhd_ctl->model_ctl->fld_ctl->field_list);
     */
}

static GtkWidget *create_box_in_bottuns(GtkWidget *tree_view, GtkWidget *window)
{
	GtkWidget *hbox;
	GtkWidget *button;
	GtkWidget *menubutton;
	GMenu *menumodel;
	GMenu *submenu[30];
    
	int i,j,k;
	char f_label[16];
	
	hbox = gtk_hbox_new(FALSE, 0);
	/* Add data bottun */
	menubutton = gtk_menu_button_new();
/*	gtk_widget_set_size_request (menubutton, 80, 35);*/
	g_signal_connect(G_OBJECT(menubutton), "activate", G_CALLBACK(append_model_data), NULL);
	
	menumodel = g_menu_new();
	for(j=0; j<NUM_FIELD/10;j++){
		submenu[j] = g_menu_new ();
		sprintf(f_label, "Field_list_%d-", j*10);
		g_menu_append_submenu (menumodel, f_label, G_MENU_MODEL(submenu[j]));
		for(k=0; k<10;k++){
			i = k + j*10;
			if(all_fld_list[i]->iflag_use == 0){
				g_menu_append (submenu[j], all_fld_list[i]->field_name, all_fld_list[i]->field_name);
			};
		};
	};
	gtk_menu_button_set_menu_model (GTK_MENU_BUTTON (menubutton), G_MENU_MODEL (menumodel));
	g_signal_connect(G_OBJECT(menubutton), "add_field", G_CALLBACK(append_model_data), NULL);
	gtk_box_pack_start(GTK_BOX(hbox), menubutton, FALSE, FALSE, 0);
	
	
	/* Delete data bottun */
	button = gtk_button_new_from_stock(GTK_STOCK_REMOVE);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(remove_model_data), tree_view);
	gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);

	/* Close window bottun */
	button = gtk_button_new_from_stock(GTK_STOCK_CLOSE);
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(cb_close_window), window);
	gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);
	
	return hbox;
};

static void set_selection_property(GtkWidget *tree_view){
	GtkTreeSelection *selection;
	GtkTreeModel *model;
	GtkTreeModel *child_model;
	gulong changed_handler_id;
	GList *list;
	
	/*
	 * selectionにchangedシグナルハンドラを登録する。
	 * 後で同じchild_modelを使用しているselectionのchangedシグナルをブロック出来るように
	 * child_modelにselectionのリストを、selectionにシグナルハンドラIDを登録する。
	 */
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view));
	changed_handler_id = g_signal_connect(G_OBJECT(selection), "changed", G_CALLBACK(sum_selected_rows), NULL);
	g_object_set_data(G_OBJECT(selection), "changed_handler_id", GUINT_TO_POINTER(changed_handler_id));

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(tree_view));
	child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
	list = g_object_get_data(G_OBJECT(child_model), "selection_list");
	list = g_list_append(list, selection);
	g_object_set_data(G_OBJECT(child_model), "selection_list", list);
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

static GtkWidget * create_field_list_window(GtkWidget *window)
{
/*	GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);*/
	GtkTreeModel *model;
	GtkWidget *tree_view;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	GtkTreeSelection *selection;
		
	GtkListStore *child_model;
	
	/* Construct empty list storage */
	child_model = gtk_list_store_new(6, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING,
				G_TYPE_INT, G_TYPE_INT, G_TYPE_INT);
	g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);

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
	renderer = gtk_cell_renderer_toggle_new();
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_attributes(column, renderer, "active", COLUMN_VIZ_FLAG, NULL);
	g_object_set(renderer, "width", (gint)60, NULL);
	gtk_tree_view_column_set_resizable(column, TRUE);
	gtk_tree_view_column_set_clickable(column, TRUE);
	g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_VIZ_FLAG));
	g_signal_connect(G_OBJECT(column), "clicked", G_CALLBACK(column_clicked), tree_view);

	/* Fifth row */
	column = gtk_tree_view_column_new();
	gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), column);
	gtk_tree_view_column_set_title(column, "Monitor output");
	renderer = gtk_cell_renderer_toggle_new();
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_attributes(column, renderer, "active", COLUMN_MONITOR_FLAG, NULL);
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
	set_selection_property(tree_view);
	
	return tree_view;
}

static void create_tree_view_window(GtkButton *button, gpointer user_data)
{
	GtkWidget *window;
	GtkWidget *tree_view;
	
	GtkWidget *vbox;
	GtkWidget *hbox;
	GtkWidget *scrolled_window;
	
	gchar *title;
	static gint window_id = 0;
	
	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	title = g_strdup_printf("GtkTreeModelSort #%d", ++window_id);
	gtk_window_set_title(GTK_WINDOW(window), title);
	g_free(title);
	
	
	hbox = create_box_in_bottuns(tree_view, window);
	
	tree_view = create_field_list_window(window);
	scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scrolled_window, 400, 300);
	gtk_container_add(GTK_CONTAINER(scrolled_window), tree_view);
	
	
	/* ウィンドウ作成 */
	
	vbox = gtk_vbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, TRUE, TRUE, 0);
	gtk_container_add(GTK_CONTAINER(window), vbox);
	
	gtk_widget_show_all(window);

};

int main(int argc, char **argv)
{
	GtkWidget *hbox;
	GtkWidget *button;

	srand((unsigned)time(NULL));

    all_fld_list = (struct all_field_ctl_c **) malloc(NUM_FIELD * sizeof(struct all_field_ctl_c *));
    alloc_all_field_ctl_c(all_fld_list);
	
	mhd_ctl = (struct SGS_MHD_control_c *) malloc(sizeof(struct SGS_MHD_control_c));
	alloc_SGS_MHD_control_c(mhd_ctl);
	read_SGS_MHD_control_file_c(file_name, buf, mhd_ctl);
    load_field_from_ctl(&mhd_ctl->model_ctl->fld_ctl->field_list, all_fld_list);
	
	
	gtk_init(&argc, &argv);

	main_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(main_window), "GtkTreeModelSort");
	g_signal_connect(G_OBJECT(main_window), "destroy", G_CALLBACK(gtk_main_quit), NULL);

	hbox = gtk_hbox_new(TRUE, 10);

	button = gtk_button_new_with_label("Create Window");
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(create_tree_view_window), NULL);
	gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);

	gtk_container_add(GTK_CONTAINER(main_window), hbox);
	gtk_container_set_border_width(GTK_CONTAINER(main_window), 10);
	gtk_widget_show_all(main_window);

	gtk_main();
	return 0;
}
