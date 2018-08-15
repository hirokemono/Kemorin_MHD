/*
//  control_panel_4_field_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/14.
*/

#include "control_panel_4_field_GTK.h"


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

static void transfer_model_data(int iflag_if_add, struct all_field_ctl_c **all_fld_tbl,
			struct field_ctl_c *fld_ctl, GtkTreeView *tree_view_to_del, GtkTreeView *tree_view_to_add)
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
               all_fld_tbl[index_field]->field_name);
		/* Delete */
		gtk_list_store_remove(GTK_LIST_STORE(child_model_to_del), &iter);
		
		/* Add */
		append_model_data(index_field, all_fld_tbl, child_model_to_add);
		
		gtk_tree_path_free(tree_path);
		gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
		
		/* Update control data */
		if(iflag_if_add == 1){
			printf("Add field list \n");
			add_field_wqflag_to_ctl(all_fld_tbl[index_field], fld_ctl);
		} else {
			printf("Delete field list \n");
			delete_field_wqflag_in_ctl(all_fld_tbl[index_field], fld_ctl);
		};
		
	}
	g_list_free(reference_list);

	/* changedシグナルのブロックを解除する */
	unblock_changed_signal(G_OBJECT(child_model_to_del));
	unblock_changed_signal(G_OBJECT(child_model_to_add));
}

static void remove_field_to_use(GtkButton *button, gpointer user_data)
{
	struct field_views *fields_vws = (struct field_views *) user_data;
	
	transfer_model_data(0, fields_vws->all_fld_tbl, fields_vws->fld_ctl_gtk,
				fields_vws->used_tree_view, fields_vws->unused_field_tree_view);
    /*
    check_field_ctl_listfields_vws->fld_ctl);
     */
}

static void add_field_to_use(GtkButton *button, gpointer user_data)
{
	struct field_views *fields_vws = (struct field_views *) user_data;
	
	transfer_model_data(0, fields_vws->all_fld_tbl, fields_vws->fld_ctl_gtk,
				fields_vws->unused_field_tree_view, fields_vws->used_tree_view);
    /*
    check_field_ctl_list(fields_vws->fld_ctl_gtk);
     */
}

static void cb_set_field_name(GtkComboBox *combobox_field, gpointer user_data)
{
	struct field_views *fields_vws = (struct field_views *) user_data;
	GtkTreeModel *model_field = gtk_combo_box_get_model(combobox_field);
	GtkTreeIter iter;
	
    gchar *row_string;
	int index_field;
	int num_comp;
	
	GtkWidget *combobox_comp;
	GtkCellRenderer *column_comp;
	GtkTreeModel *model_comp;
	
	gint idx = gtk_combo_box_get_active(combobox_field);
    if(idx < 0) return;
    
	GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
	
    gtk_tree_model_get_iter(model_field, &iter, path);  
    gtk_tree_model_get(model_field, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_field, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_field, &iter, COLUMN_NUM_COMP, &num_comp, -1);
    
    printf("Selected field %d %d, %s\n", index_field, num_comp, row_string);
	
	if(num_comp == 6){
		model_comp =  gtk_tree_view_get_model(GTK_TREE_VIEW(fields_vws->sym_tensor_label_view));
	}else if(num_comp == 3){
		model_comp =  gtk_tree_view_get_model(GTK_TREE_VIEW(fields_vws->vector_label_view));
	}else{
		model_comp =  gtk_tree_view_get_model(GTK_TREE_VIEW(fields_vws->scalar_label_view));
	};
	
	combobox_comp = g_object_get_data(G_OBJECT(combobox_field), "cbox_comp");
	gtk_combo_box_set_model(combobox_comp, model_comp);
	
	return;
}

static void cb_set_component_name(GtkComboBox *combobox_comp, gpointer user_data)
{
	GtkTreeModel *model_comp = gtk_combo_box_get_model(combobox_comp);  
	GtkTreeIter iter;
	
	gchar *row_string;
	int index_comp;
	int num_comp;
	
	gint idx = gtk_combo_box_get_active(combobox_comp);
    if(idx < 0) return;
    
	GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
	
    gtk_tree_model_get_iter(model_comp, &iter, path);  
    gtk_tree_model_get(model_comp, &iter, COLUMN_FIELD_INDEX, &index_comp, -1);
    gtk_tree_model_get(model_comp, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    
    printf("Selected component %d, %s\n", index_comp, row_string);
	
	return;
}

void add_field_selection_box(struct field_views *fields_vws, GtkWidget *vbox)
{
	GtkWidget *hbox;
	GtkWidget *button;
	GtkWidget *label;
	GtkWidget *scrolled_window;
	
	GtkTreeSelection *selection;
	GtkTreeModel *model;
	GtkTreeModel *child_model;
	gulong changed_handler_id;
	GList *list;
	
	GtkWidget *expander;
	GtkWidget *hbox_1, *vbox_1, *Frame_1;
	GtkWidget *label_1;
	GtkWidget *scrolled_window_1;
	
	GtkTreeSelection *selection_1;
	GtkTreeModel *model_1;
	GtkTreeModel *child_model_1;
	gulong changed_handler_id_1;
	GList *list_1;
	
	char *c_label;
	
	c_label = (char *)calloc(KCHARA_C, sizeof(char));
	
	
	hbox = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

	/* Delete data bottun */
	button = gtk_button_new_from_stock(GTK_STOCK_REMOVE);
	gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);
	g_signal_connect(G_OBJECT(button), "clicked", 
				G_CALLBACK(remove_field_to_use), fields_vws);

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
	
};

void add_field_combobox_vbox(struct field_views *fields_vws, GtkWidget *vbox)
{
	GtkTreeModel *model_field;
	GtkTreeModel *model_comp;
	GtkWidget *hbox;
	GtkWidget *combobox_field;
    GtkCellRenderer *column_field;
	GtkWidget *combobox_comp;
    GtkCellRenderer *column_comp;
	
	model_field = gtk_tree_view_get_model(GTK_TREE_VIEW(fields_vws->used_tree_view));
	model_comp =  gtk_tree_view_get_model(GTK_TREE_VIEW(fields_vws->vector_label_view));
	
	hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	combobox_field = gtk_combo_box_new_with_model(model_field);
	g_signal_connect(G_OBJECT(combobox_field), "changed", 
				G_CALLBACK(cb_set_field_name), fields_vws);
	column_field = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_field), column_field, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_field), column_field,
				"text", COLUMN_FIELD_NAME, NULL);
	gtk_box_pack_start(GTK_BOX(hbox), combobox_field, FALSE, FALSE, 0);
	
	
	combobox_comp = gtk_combo_box_new_with_model(model_comp);
	g_signal_connect(G_OBJECT(combobox_comp), "changed", 
				G_CALLBACK(cb_set_component_name), NULL);
	
	g_object_set_data(G_OBJECT(combobox_field), "cbox_comp", combobox_comp);
	g_object_set_data(G_OBJECT(combobox_field), "column_comp", combobox_comp);
	gtk_box_pack_start(GTK_BOX(hbox), combobox_comp, FALSE, FALSE, 0);
	
	column_comp = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_comp), column_comp, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_comp), column_comp,
				"text", COLUMN_FIELD_NAME, NULL);
	
	
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
}
