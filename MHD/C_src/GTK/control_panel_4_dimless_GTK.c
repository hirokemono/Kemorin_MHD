/*
//  control_panel_4_dimless_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#include "control_panel_4_dimless_GTK.h"

static void set_last_field_to_label(GtkTreeSelection *selection, gpointer user_data)
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
        set_last_field_to_label(selection, NULL);
    }
}

static void delete_dless_data(struct dimless_ctl_c *dless_ctl_gtk,
                                GtkTreeView *tree_view_to_del)
{
    GtkTreeModel *model_to_del;
    GtkTreeModel *child_model_to_del;
    GtkTreeSelection *selection;
    GList *list;
    GList *reference_list;
    GList *cur;
    
    gchar *field_name;
    gchar *field_math;
    int index_field;
    double value;
    
    /* 選択されている行のパスを取得する */
    /* パスはツリーモデルソートのもの */
    model_to_del = gtk_tree_view_get_model(tree_view_to_del);
    child_model_to_del = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_del));
        
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
    
    /* リファレンスをパスに戻して削除 */
    for (cur = g_list_first(reference_list); cur != NULL; cur = g_list_next(cur)) {
        GtkTreePath *tree_path;
        GtkTreeIter iter;
        tree_path = gtk_tree_row_reference_get_path((GtkTreeRowReference *)cur->data);
        gtk_tree_model_get_iter(child_model_to_del, &iter, tree_path);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_NAME, &field_name, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_MATH, &field_math, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_VALUE, &value, -1);
        
        printf("To be moved: %d, %s %lf\n", index_field, field_name, value);
        /* Delete */
        gtk_list_store_remove(GTK_LIST_STORE(child_model_to_del), &iter);

        gtk_tree_path_free(tree_path);
        gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
        
        /* Update control data */
        del_chara_real_ctl_list_by_c_tbl(field_name, &dless_ctl_gtk->dimless_list);
    }
    g_list_free(reference_list);
    
    /* changedシグナルのブロックを解除する */
    unblock_changed_signal(G_OBJECT(child_model_to_del));
}

static void remove_field_to_use(GtkButton *button, gpointer user_data)
{
    struct dimless_views *dless_vws = (struct dimless_views *) user_data;

    delete_dless_data(dless_vws->dless_ctl_gtk, dless_vws->dimless_tree_view);
    /*
     write_chara_real_ctl_list(stdout, 0, "Added list", &dless_vws->dless_ctl_gtk->dimless_list);
     */
}

static void cb_set_dimless_name(GtkComboBox *combobox_field, gpointer user_data)
{
    struct dimless_views *dless_vws = (struct dimless_views *) user_data;
    GtkTreeModel *model_field = gtk_combo_box_get_model(combobox_field);
    GtkTreeIter iter;
    
    gchar *row_string;
    int index_field;
    
    gint idx = gtk_combo_box_get_active(combobox_field);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_field, &iter, path);  
    gtk_tree_model_get(model_field, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_field, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    
    printf("Selected field %d, %s\n", index_field, row_string);
    
    return;
}

static void cb_add_dimless_name(GtkComboBox *combobox_add, gpointer user_data)
{
    struct dimless_views *dless_vws = (struct dimless_views *) user_data;
    GtkTreeModel *model_to_add = gtk_tree_view_get_model(dless_vws->dimless_tree_view);
    GtkTreeModel *child_model_to_add = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_add));
    
    GtkTreeModel *model_comp = gtk_combo_box_get_model(combobox_add);  

    GtkTreeIter iter;
    
    gchar *row_string;
    gchar *math_string;
    int index_comp;
    double value;
    
    gint idx = gtk_combo_box_get_active(combobox_add);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_comp, &iter, path);  
    gtk_tree_model_get(model_comp, &iter, COLUMN_FIELD_INDEX, &index_comp, -1);
    gtk_tree_model_get(model_comp, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_comp, &iter, COLUMN_FIELD_MATH, &math_string, -1);
    gtk_tree_model_get(model_comp, &iter, COLUMN_FIELD_VALUE, &value, -1);

    dless_vws->index_dless = append_dimless_item(dless_vws->index_dless,
                         row_string, math_string, value, child_model_to_add);

    append_chara_real_ctl_list(row_string, value, &dless_vws->dless_ctl_gtk->dimless_list);
/*
    write_chara_real_ctl_list(stdout, 0, "Added list", &dless_vws->dless_ctl_gtk->dimless_list);
*/
    return;
}

void add_dimless_selection_box(struct dimless_views *dless_vws, GtkWidget *vbox)
{
    GtkWidget *hbox;
    GtkWidget *button;

    GtkTreeModel *model_comp;
    GtkWidget *combobox_add;
    GtkCellRenderer *column_add;
    
    GtkWidget *label;
    GtkWidget *scrolled_window;
    
    GtkTreeSelection *selection;
    GtkTreeModel *model;
    GtkTreeModel *child_model;
    gulong changed_handler_id;
    GList *list;
    
    char *c_label;
    
    c_label = (char *)calloc(KCHARA_C, sizeof(char));
    
    model_comp =  gtk_tree_view_get_model(GTK_TREE_VIEW(dless_vws->default_dless_view));
   
    hbox = gtk_hbox_new(FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
    
    /* Add data combocox */
    label = gtk_label_new("Add: ");
    gtk_box_pack_start(GTK_BOX(hbox), label, TRUE, TRUE, 0);
    combobox_add = gtk_combo_box_new_with_model(model_comp);
    g_signal_connect(G_OBJECT(combobox_add), "changed", 
                     G_CALLBACK(cb_add_dimless_name), dless_vws);
    gtk_box_pack_start(GTK_BOX(hbox), combobox_add, FALSE, FALSE, 0);
    /* Delete data bottun */
    button = gtk_button_new_from_stock(GTK_STOCK_REMOVE);
    g_signal_connect(G_OBJECT(button), "clicked", 
                     G_CALLBACK(remove_field_to_use), dless_vws);
    gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);
    
    column_add = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_add), column_add, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_add), column_add,
                                   "text", COLUMN_FIELD_NAME, NULL);
    
    /* ラベル */
    label = gtk_label_new("");
    gtk_box_pack_end(GTK_BOX(hbox), label, TRUE, TRUE, 0);
    
    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(scrolled_window, 400, 300);
    gtk_container_add(GTK_CONTAINER(scrolled_window), dless_vws->dimless_tree_view);
    gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, TRUE, TRUE, 0);
    
    /*
     * selectionにchangedシグナルハンドラを登録する。
     * 後で同じchild_modelを使用しているselectionのchangedシグナルをブロック出来るように
     * child_modelにselectionのリストを、selectionにシグナルハンドラIDを登録する。
     * changedハンドラ内で使用するlabelも同様に登録しておく。
     */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(dless_vws->dimless_tree_view));
    changed_handler_id = g_signal_connect(G_OBJECT(selection), "changed",
                                          G_CALLBACK(set_last_field_to_label), NULL);
    g_object_set_data(G_OBJECT(selection), "changed_handler_id", GUINT_TO_POINTER(changed_handler_id));
    g_object_set_data(G_OBJECT(selection), "label", label);
    
    model = gtk_tree_view_get_model(GTK_TREE_VIEW(dless_vws->dimless_tree_view));
    child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    list = g_object_get_data(G_OBJECT(child_model), "selection_list");
    list = g_list_append(list, selection);
    g_object_set_data(G_OBJECT(child_model), "selection_list", list);
};

void add_dimless_combobox_vbox(struct dimless_views *dless_vws, GtkWidget *vbox)
{
    GtkTreeModel *model_field;
    GtkWidget *hbox;
    GtkWidget *combobox_field;
    GtkCellRenderer *column_field;
    
    model_field = gtk_tree_view_get_model(GTK_TREE_VIEW(dless_vws->dimless_tree_view));
    
    hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
    combobox_field = gtk_combo_box_new_with_model(model_field);
    g_signal_connect(G_OBJECT(combobox_field), "changed", 
                     G_CALLBACK(cb_set_dimless_name), dless_vws);
    column_field = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_field), column_field, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_field), column_field,
                                   "text", COLUMN_FIELD_NAME, NULL);
    gtk_box_pack_start(GTK_BOX(hbox), combobox_field, FALSE, FALSE, 0);

    
    gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
}
