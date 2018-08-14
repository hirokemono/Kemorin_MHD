#include <stdlib.h>
#include <gtk/gtk.h>

#include "t_ctl_data_4_fields_c.h"
#include "t_SGS_MHD_control_c.h"

struct all_field_ctl_c **all_fld_list;
struct SGS_MHD_control_c *mhd_ctl;
char file_name[LENGTHBUF] = "/Users/matsui/work/C_test/control_MHD";
char buf[LENGTHBUF];      /* character buffer for reading line */

static GtkWidget *main_window = NULL;

struct field_views{
	GtkWidget *used_tree_view;
	GtkWidget *unused_field_tree_view;
};

enum {
	COLUMN_FIELD_INDEX = 0,
	COLUMN_FIELD_NAME,
	COLUMN_FIELD_MATH,
	COLUMN_VIZ_FLAG,
	COLUMN_MONITOR_FLAG,
	COLUMN_NUM_COMP,
	COLUMN_QUADRATURE,
};


static void sum_selected_rows(GtkTreeSelection *selection, gpointer user_data)
{
	GtkLabel *label;
	GtkTreeModel *model;
	GList *list;
	GList *cur;
    gchar *row_string;
	GtkTreeIter iter;

	label = g_object_get_data(G_OBJECT(selection), "label");

	list = gtk_tree_selection_get_selected_rows(selection, &model);
	if (list == NULL) {
		gtk_label_set_text(label, "");
		return;
	}
	
	for (cur = g_list_first(list); cur != NULL; cur = g_list_next(cur)) {
		if (gtk_tree_model_get_iter(model, &iter, (GtkTreePath *)cur->data) == TRUE) {
			gtk_tree_model_get(model, &iter, COLUMN_FIELD_NAME, &row_string, -1);
		}
		gtk_tree_path_free((GtkTreePath *)cur->data);
	}
	g_list_free(list);
	
	gtk_label_set_text(label, row_string);
}

static void cb_close_window(GtkButton *button, gpointer user_data){
	GtkWidget *window = (GtkWidget *) user_data;
	gtk_widget_destroy(window);
	gtk_widget_show_all(main_window);
};


/* Append new data at the end of list */
static void append_model_data(int index_field, GtkTreeModel *child_model)
{
	GtkTreeIter iter;

	gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
	gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
					   COLUMN_FIELD_INDEX, index_field,
					   COLUMN_FIELD_NAME, all_fld_list[index_field]->field_name,
					   COLUMN_FIELD_MATH, all_fld_list[index_field]->field_math,
					   COLUMN_VIZ_FLAG, (gboolean) all_fld_list[index_field]->iflag_viz,
					   COLUMN_MONITOR_FLAG, (gboolean) all_fld_list[index_field]->iflag_monitor,
					   COLUMN_NUM_COMP, all_fld_list[index_field]->num_comp,
					   COLUMN_QUADRATURE, all_fld_list[index_field]->iflag_quad,
						-1);
}

static void block_changed_signal(GObject *instance)
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

static void unblock_changed_signal(GObject *instance)
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

		/* changedシグナルをブロックしていた間の変更を反映させる */
		sum_selected_rows(selection, NULL);
	}
}

static void transfer_model_data(int iflag_if_add, 
			GtkTreeView *tree_view_to_del, GtkTreeView *tree_view_to_add)
{
	GtkTreeModel *model_to_del;
	GtkTreeModel *child_model_to_del;
	GtkTreeModel *model_to_add;
	GtkTreeModel *child_model_to_add;
	GtkTreeSelection *selection;
	GList *list;
	GList *reference_list;
	GList *cur;
    
    gchar *field_name;
    gchar *field_math;
    int index_field;
    int iflag_viz, iflag_monitor, num_comp, iflag_quad;

	/* 選択されている行のパスを取得する */
	/* パスはツリーモデルソートのもの */
	model_to_del = gtk_tree_view_get_model(tree_view_to_del);
	child_model_to_del = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_del));
	
	model_to_add = gtk_tree_view_get_model(tree_view_to_add);
	child_model_to_add = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_add));
	
	selection = gtk_tree_view_get_selection(tree_view_to_del);
	list = gtk_tree_selection_get_selected_rows(selection, NULL);

	/* 最初にパスからリファレンスを作成する */
	/* データの削除を行なうと取得済みのパスが(大抵の場合)無効になる */
	reference_list = NULL;
	for (cur = g_list_first(list); cur != NULL; cur = g_list_next(cur)) {
		GtkTreePath *child_path;
		GtkTreeRowReference *child_reference;
		/* ツリーモデルソートのパスをツリーモデルのパスに変換する */
		child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model_to_del), 
					(GtkTreePath *)cur->data);

		child_reference = gtk_tree_row_reference_new(child_model_to_del, child_path);
		reference_list = g_list_append(reference_list, child_reference);

		gtk_tree_path_free(child_path);
		gtk_tree_path_free((GtkTreePath *)cur->data);
	}
	g_list_free(list);

	/* GtkTreeSelectionのchangedシグナルを一時的にブロックする */
	block_changed_signal(G_OBJECT(child_model_to_del));
	block_changed_signal(G_OBJECT(child_model_to_add));

	/* リファレンスをパスに戻して削除 */
	for (cur = g_list_first(reference_list); cur != NULL; cur = g_list_next(cur)) {
		GtkTreePath *tree_path;
		GtkTreeIter iter;
		tree_path = gtk_tree_row_reference_get_path((GtkTreeRowReference *)cur->data);
		gtk_tree_model_get_iter(child_model_to_del, &iter, tree_path);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_NAME, &field_name, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_MATH, &field_math, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_VIZ_FLAG, &iflag_viz, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_MONITOR_FLAG, &iflag_monitor, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_NUM_COMP, &num_comp, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_QUADRATURE, &iflag_quad, -1);
        
        printf("To be moved: %d, %s: %s\n", index_field, field_name,
               all_fld_list[index_field]->field_name);
		/* Delete */
		gtk_list_store_remove(GTK_LIST_STORE(child_model_to_del), &iter);
		
		/* Add */
		append_model_data(index_field, child_model_to_add);
		
		gtk_tree_path_free(tree_path);
		gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
		
		/* Update control data */
		if(iflag_if_add == 1){
			printf("Add field list \n");
			add_field_to_ctl(all_fld_list[index_field], &mhd_ctl->model_ctl->fld_ctl->field_list);
		} else {
			printf("Delete field list \n");
			delete_field_in_ctl(all_fld_list[index_field], &mhd_ctl->model_ctl->fld_ctl->field_list);
		};
		
	}
	g_list_free(reference_list);

	/* changedシグナルのブロックを解除する */
	unblock_changed_signal(G_OBJECT(child_model_to_del));
	unblock_changed_signal(G_OBJECT(child_model_to_add));
    /*
    check_field_ctl_list(&mhd_ctl->model_ctl->fld_ctl->field_list);
     */
}

static void remove_field_to_use(GtkButton *button, gpointer user_data)
{
	struct field_views *fields_vws = (struct field_views *) user_data;
	
	transfer_model_data(0, fields_vws->used_tree_view, fields_vws->unused_field_tree_view);
    /*
    check_field_ctl_list(&mhd_ctl->model_ctl->fld_ctl->field_list);
     */
}

static void add_field_to_use(GtkButton *button, gpointer user_data)
{
	struct field_views *fields_vws = (struct field_views *) user_data;
	
	transfer_model_data(1, fields_vws->unused_field_tree_view, fields_vws->used_tree_view);
    /*
    check_field_ctl_list(&mhd_ctl->model_ctl->fld_ctl->field_list);
     */
}

void format_entry_text_callback (GtkComboBox *combobox, gpointer user_data)
{
    GtkTreeView *tree_view = GTK_TREE_VIEW(user_data);
    GtkTreeModel *model = gtk_tree_view_get_model (tree_view);  
	GtkTreeIter iter;
	
    gchar *row_string;
	int index_field;
	
	gint idx = gtk_combo_box_get_active(combobox);
    if(idx < 0) return;
    
	GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
	
    gtk_tree_model_get_iter(model, &iter, path);  
    gtk_tree_model_get(model, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    
    printf("Selected field %d, %s: %s\n", index_field, row_string,
				all_fld_list[index_field]->field_name);
	
	return;
}

static GtkWidget *create_window(struct field_views *fields_vws, GtkWidget *window)
{
	GtkWidget *vbox;
	GtkWidget *hbox;
	GtkWidget *button;
	GtkWidget *label;
	GtkWidget *scrolled_window;
	GtkWidget *combobox_field;
	GtkWidget *combobox_comp;
	
	GtkTreeSelection *selection;
	GtkTreeModel *model;
	GtkTreeModel *child_model;
	gulong changed_handler_id;
	GList *list;
	
	GtkWidget *expander;
	GtkWidget *hbox_1, *vbox_1, *Frame_1;
	GtkWidget *label_1;
	GtkWidget *scrolled_window_1;
	GtkWidget *hbox_11;
	
	GtkTreeSelection *selection_1;
	GtkTreeModel *model_1;
	GtkTreeModel *child_model_1;
	gulong changed_handler_id_1;
	GList *list_1;
	
	
	
    GtkCellRenderer *column;
	
	char *c_label;
	
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	
	
	vbox = gtk_vbox_new(FALSE, 0);

	hbox = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

	/* Delete data bottun */
	button = gtk_button_new_from_stock(GTK_STOCK_REMOVE);
	gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(button), "clicked", 
				G_CALLBACK(remove_field_to_use), fields_vws);

	/* Close window bottun */
	button = gtk_button_new_from_stock(GTK_STOCK_CLOSE);
	gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(button), "clicked", 
				G_CALLBACK(cb_close_window), window);

	/* ラベル */
	label = gtk_label_new("");
	gtk_box_pack_end(GTK_BOX(hbox), label, TRUE, TRUE, 0);

	scrolled_window = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_widget_set_size_request(scrolled_window, 400, 300);
	gtk_container_add(GTK_CONTAINER(scrolled_window), fields_vws->used_tree_view);
	gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, TRUE, TRUE, 0);

	/*
	 * selectionにchangedシグナルハンドラを登録する。
	 * 後で同じchild_modelを使用しているselectionのchangedシグナルをブロック出来るように
	 * child_modelにselectionのリストを、selectionにシグナルハンドラIDを登録する。
	 * changedハンドラ内で使用するlabelも同様に登録しておく。
	 */
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(fields_vws->used_tree_view));
	changed_handler_id = g_signal_connect(G_OBJECT(selection), "changed",
				G_CALLBACK(sum_selected_rows), NULL);
	g_object_set_data(G_OBJECT(selection), "changed_handler_id", GUINT_TO_POINTER(changed_handler_id));
	g_object_set_data(G_OBJECT(selection), "label", label);

	model = gtk_tree_view_get_model(GTK_TREE_VIEW(fields_vws->used_tree_view));
	child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
	list = g_object_get_data(G_OBJECT(child_model), "selection_list");
	list = g_list_append(list, selection);
	g_object_set_data(G_OBJECT(child_model), "selection_list", list);
	
	
	get_label_MHD_control_head(c_label);
	expander = gtk_expander_new_with_mnemonic("Field to add");
	
	vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	Frame_1 = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_1), GTK_SHADOW_IN);
	
	scrolled_window_1 = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window_1),
				GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
//	gtk_widget_set_size_request(scrolled_window_1, 400, 300);
	gtk_container_add(GTK_CONTAINER(scrolled_window_1), fields_vws->unused_field_tree_view);
	gtk_box_pack_start(GTK_BOX(vbox_1), scrolled_window_1, TRUE, TRUE, 0);

	
	
	gtk_container_add(GTK_CONTAINER(Frame_1), vbox_1);
	
	/* Add data bottun */
	hbox_1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	button = gtk_button_new_from_stock(GTK_STOCK_ADD);
	gtk_box_pack_start(GTK_BOX(hbox_1), button, FALSE, TRUE, 0);
	g_signal_connect(G_OBJECT(button), "clicked", 
				G_CALLBACK(add_field_to_use), fields_vws);
	label_1 = gtk_label_new("");
	gtk_box_pack_end(GTK_BOX(hbox_1), label_1, TRUE, TRUE, 0);

	gtk_box_pack_start(GTK_BOX(hbox_1), gtk_label_new("  "), FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1), Frame_1, TRUE, TRUE, 0);
	gtk_container_add(GTK_CONTAINER(expander), hbox_1);
	gtk_box_pack_start(GTK_BOX(vbox), expander, TRUE, TRUE, 0);
	
	
	
	selection_1 = gtk_tree_view_get_selection(GTK_TREE_VIEW(fields_vws->unused_field_tree_view));
	changed_handler_id_1 = g_signal_connect(G_OBJECT(selection_1), "changed", 
				G_CALLBACK(sum_selected_rows), NULL);
	g_object_set_data(G_OBJECT(selection_1), "changed_handler_id", GUINT_TO_POINTER(changed_handler_id_1));
	g_object_set_data(G_OBJECT(selection_1), "label", label_1);

	model_1 = gtk_tree_view_get_model(GTK_TREE_VIEW(fields_vws->unused_field_tree_view));
	child_model_1 = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_1));
	list_1 = g_object_get_data(G_OBJECT(child_model_1), "selection_list");
	list_1 = g_list_append(list_1, selection_1);
	g_object_set_data(G_OBJECT(child_model_1), "selection_list", list_1);
	
	
	
	hbox_11 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	combobox_field = gtk_combo_box_new_with_model(model);
	g_signal_connect(G_OBJECT(combobox_field), "changed", 
				G_CALLBACK(format_entry_text_callback), fields_vws->used_tree_view);
	column = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_field), column, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_field), column,
				"text", COLUMN_FIELD_NAME, NULL);
	gtk_box_pack_start(GTK_BOX(hbox_11), combobox_field, FALSE, FALSE, 0);
	
	combobox_comp = gtk_combo_box_new();
	g_signal_connect(G_OBJECT(combobox_comp), "changed", 
				G_CALLBACK(format_entry_text_callback), fields_vws->used_tree_view);
	column = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_comp), column, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_comp), column,
				"text", COLUMN_FIELD_NAME, NULL);
	gtk_box_pack_start(GTK_BOX(hbox_11), combobox_comp, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox), hbox_11, FALSE, FALSE, 0);
	gtk_container_add(GTK_CONTAINER(window), vbox);

	return window;
}

static void unused_column_clicked(GtkTreeViewColumn *column, gpointer user_data)
{
	GtkTreeView *unused_field_tree_view = GTK_TREE_VIEW(user_data);
	GtkTreeModel *model;
	gint unused_column_id;
	gint cur_id;
	GtkSortType order;
	GtkTreeViewColumn *cur_column;
	
	GtkTreeViewColumn *button;
	
	if (gtk_widget_is_focus(GTK_WIDGET(unused_field_tree_view)) == FALSE) {
		gtk_widget_grab_focus(GTK_WIDGET(unused_field_tree_view));
	}
	
	unused_column_id = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(column), "unused_column_id"));
	model = gtk_tree_view_get_model(unused_field_tree_view);
	
	/* 現在のソート列と同じときは昇順／降順を反転する、違うときはクリックした列で昇順ソートする */
	if (gtk_tree_sortable_get_sort_column_id(GTK_TREE_SORTABLE(model), &cur_id, &order) == TRUE) {
		if (cur_id == unused_column_id) {
			order = (order == GTK_SORT_ASCENDING) ? GTK_SORT_DESCENDING : GTK_SORT_ASCENDING;
		} else {
			order = GTK_SORT_ASCENDING;
		}
		cur_column = gtk_tree_view_get_column(unused_field_tree_view, cur_id);
		gtk_tree_view_column_set_sort_indicator(cur_column, FALSE);
	} else {
		order = GTK_SORT_ASCENDING;
	}
	gtk_tree_view_column_set_sort_order(column, order);
	gtk_tree_view_column_set_sort_indicator(column, TRUE);
	gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), unused_column_id, order);
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

static void toggle_viz_switch(GtkTreeViewColumn *renderer, 
			gchar *path_str, gpointer user_data){
    gchar *row_string;
    int index_field, index_for_toggle;

    GtkTreeView *tree_view = GTK_TREE_VIEW(user_data);
    GtkTreeModel *model = gtk_tree_view_get_model (tree_view);  
	GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;

    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_VIZ_FLAG, &index_for_toggle, -1);
    
   
    printf("toggle_viz_switch %d, %s: %s\n", index_field, row_string,
            all_fld_list[index_field]->field_name);
    
    index_for_toggle = (index_for_toggle+ 1) % 2;
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_VIZ_FLAG, (gboolean) index_for_toggle, -1);
    gtk_tree_path_free(child_path);  
    gtk_tree_path_free(path);  
    
    all_fld_list[index_field]->iflag_viz = index_for_toggle;
    update_field_flag_in_ctl(all_fld_list[index_field], &mhd_ctl->model_ctl->fld_ctl->field_list);
}
static void toggle_monitor_switch(GtkTreeViewColumn *renderer, gchar *path_str, gpointer user_data){
    gchar *row_string;
    int index_field, index_for_toggle;
    
    GtkTreeView *tree_view = GTK_TREE_VIEW(user_data);
    GtkTreeModel *model = gtk_tree_view_get_model (tree_view);  
	GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;

    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &row_string, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_MONITOR_FLAG, &index_for_toggle, -1);
    
    printf("toggle_viz_switch %d, %s: %s\n", index_field, row_string,
           all_fld_list[index_field]->field_name);
	
    index_for_toggle = (index_for_toggle+ 1) % 2;
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_MONITOR_FLAG, (gboolean) index_for_toggle, -1);
    gtk_tree_path_free(child_path);  
    gtk_tree_path_free(path);  
    
    all_fld_list[index_field]->iflag_monitor = index_for_toggle;
    update_field_flag_in_ctl(all_fld_list[index_field], &mhd_ctl->model_ctl->fld_ctl->field_list);
}

static void create_field_tree_view(GtkWidget *tree_view)
{
/*	GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);*/
	GtkTreeModel *model;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	GtkTreeSelection *selection;
		
	GtkListStore *child_model;
	
	int i;
	
	/* Construct empty list storage */
	child_model = gtk_list_store_new(7, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING,
				G_TYPE_INT, G_TYPE_INT, G_TYPE_INT, G_TYPE_INT);
	g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);

	/* ソート用のモデルを作成してツリービューにセットする */
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
    g_signal_connect(G_OBJECT(renderer), "toggled", G_CALLBACK(toggle_viz_switch), tree_view);
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
    g_signal_connect(G_OBJECT(renderer), "toggled", G_CALLBACK(toggle_monitor_switch), tree_view);
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
	
	/*init_model_data(tree_view);*/
	for(i=0;i<NUM_FIELD;i++){
		if(all_fld_list[i]->iflag_use > 0) {append_model_data(i, child_model);};
	}
	
}

static void create_unused_field_tree_view(GtkWidget *unused_field_tree_view)
{
/*	GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);*/
	GtkTreeModel *model;
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;
	GtkTreeSelection *selection;
		
	GtkListStore *child_model;
	
	int i;
	
	/* Construct empty list storage */
	child_model = gtk_list_store_new(7, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING,
				G_TYPE_INT, G_TYPE_INT, G_TYPE_INT, G_TYPE_INT);
	g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);

	/* ソート用のモデルを作成してツリービューにセットする */
	model = gtk_tree_model_sort_new_with_model(child_model);
	gtk_tree_view_set_model(GTK_TREE_VIEW(unused_field_tree_view), model);

	/* First raw */
	column = gtk_tree_view_column_new();
	gtk_tree_view_append_column(GTK_TREE_VIEW(unused_field_tree_view), column);
	gtk_tree_view_column_set_title(column, "Index");
	renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_FIELD_INDEX, NULL);
	g_object_set(renderer, "width", (gint)60, NULL);
	gtk_tree_view_column_set_resizable(column, TRUE);
	gtk_tree_view_column_set_clickable(column, TRUE);
	g_object_set_data(G_OBJECT(column), "unused_column_id", GINT_TO_POINTER(COLUMN_FIELD_INDEX));
	g_signal_connect(G_OBJECT(column), "clicked", G_CALLBACK(unused_column_clicked), unused_field_tree_view);

	/* Second row */
	column = gtk_tree_view_column_new();
	gtk_tree_view_append_column(GTK_TREE_VIEW(unused_field_tree_view), column);
	gtk_tree_view_column_set_title(column, "Field name");
	renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_FIELD_NAME, NULL);
	g_object_set(renderer, "width", (gint)150, NULL);
	gtk_tree_view_column_set_resizable(column, TRUE);
	gtk_tree_view_column_set_clickable(column, TRUE);
	g_object_set_data(G_OBJECT(column), "unused_column_id", GINT_TO_POINTER(COLUMN_FIELD_NAME));
	g_signal_connect(G_OBJECT(column), "clicked", G_CALLBACK(unused_column_clicked), unused_field_tree_view);

	/* 選択モード */
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(unused_field_tree_view));
	gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);

	/* 1行毎に背景色を変更 */
	gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(unused_field_tree_view), TRUE);

	/* ソート */
	column = gtk_tree_view_get_column(GTK_TREE_VIEW(unused_field_tree_view), COLUMN_FIELD_INDEX);
	gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
	gtk_tree_view_column_set_sort_indicator(column, TRUE);
	gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_INDEX, GTK_SORT_ASCENDING);
	
	/*init_model_data(unused_field_tree_view);*/
	for(i=0;i<NUM_FIELD;i++){
		if(all_fld_list[i]->iflag_use == 0) {append_model_data(i, child_model);};
	}
	
}


static void create_tree_view_window(GtkButton *button, gpointer user_data)
{
	static gint window_id = 0;
	GtkWidget *window;
	struct field_views *fields_vws;
	GtkWidget *tree_view;
	GtkWidget *unused_field_tree_view;
	
	gchar *title;
	
    fields_vws = (struct field_views *) malloc(sizeof(struct field_views));
	
	fields_vws->used_tree_view = gtk_tree_view_new();
	create_field_tree_view(fields_vws->used_tree_view);
	/* ウィンドウ作成 */
	
	fields_vws->unused_field_tree_view = gtk_tree_view_new();
	
	create_unused_field_tree_view(fields_vws->unused_field_tree_view);
	
	
	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	title = g_strdup_printf("GtkTreeModelSort #%d", ++window_id);
	gtk_window_set_title(GTK_WINDOW(window), title);
	g_free(title);
	
	create_window(fields_vws, window);
	
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
