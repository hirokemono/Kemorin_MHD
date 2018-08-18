/*
//  tree_view_real2_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "tree_view_real2_GTK.h"

void set_last_field_to_label(GtkTreeSelection *selection, gpointer user_data)
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

/* Append new data at the end of list */
int append_r2_item_to_tree(int index, char *c_tbl, char *c_math, double r_data, 
                           GtkTreeModel *child_model)
{
    GtkTreeIter iter;
    
    gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_INDEX, index,
                       COLUMN_FIELD_NAME,  c_tbl,
                       COLUMN_FIELD_MATH,  c_math,
                       COLUMN_FIELD_VALUE, r_data,
                       -1);
    return index + 1;
}

int append_r2_list_from_ctl(int index, struct chara_real_ctl_list *head, 
                            GtkTreeView *r2_tree_view)
{
    GtkTreeModel *model = gtk_tree_view_get_model (r2_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    head = head->_next;
    while (head != NULL){
        index = append_r2_item_to_tree(index, head->cr_item->c_tbl, head->cr_item->c_tbl, 
                                       head->cr_item->r_data, child_model);
        head = head->_next;
    };
    return index;
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


void cr_tree_value_edited(gchar *path_str, gchar *new_text, 
                          GtkTreeView *r2_tree_view, struct chara_real_clist *cr_clist)
{
    GtkTreeModel *model = gtk_tree_view_get_model (r2_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;
    
    gchar *old_text;
    gchar *math_string;
    double old_value, new_value;
    
    sscanf(new_text, "%lf", &new_value);
    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &old_text, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_MATH, &math_string, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_VALUE, &old_value, -1);
    
    printf("Change %lf to %lf\n", old_value, new_value);
    
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_VALUE, new_value, -1);
    gtk_tree_path_free(child_path);  
    gtk_tree_path_free(path);  
    
    update_chara_real_clist_by_c_tbl(old_text, old_text, new_value, cr_clist);
    
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

int add_r2_list_by_bottun_GTK(int index, GtkTreeView *tree_view_to_add, 
                              struct chara_real_clist *cr_clist)
{
    GtkTreeModel *model_to_add = gtk_tree_view_get_model(tree_view_to_add);
    GtkTreeModel *child_model_to_add = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_add));
    
    gchar row_string[30] = "new_number";
    gchar math_string[30] = "$$    $$";
    double value = 0.0;
    
    index = append_r2_item_to_tree(index, row_string, math_string, value, child_model_to_add);
    append_chara_real_clist(row_string, value, cr_clist);
    
    return index;
}

void delete_r2_list_items_GTK(GtkTreeView *tree_view_to_del,
                              struct chara_real_clist *cr_clist)
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
        del_chara_real_clist_by_c_tbl(field_name, cr_clist);
    }
    g_list_free(reference_list);
    
    /* changedシグナルのブロックを解除する */
    unblock_changed_signal(G_OBJECT(child_model_to_del));
}


void create_real2_tree_view(GtkTreeView *r2_tree_view,
                                GtkCellRenderer *renderer_spin1, GtkCellRenderer *renderer_spin2)
{
    /*    GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);*/
    
    GtkTreeModel *model;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    
    GtkListStore *child_model;
    
    GtkAdjustment *adjust;
    
    int i;
    
    /* Construct empty list storage */
    child_model = gtk_list_store_new(2, G_TYPE_DOUBLE, G_TYPE_DOUBLE);
    g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);
    
    /* ソート用のモデルを作成してツリービューにセットする */
    model = gtk_tree_model_sort_new_with_model(child_model);
    gtk_tree_view_set_model(GTK_TREE_VIEW(r2_tree_view), model);
    
    /* First raw */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(r2_tree_view, column);
    gtk_tree_view_column_set_title(column, "Value");
    adjust = gtk_adjustment_new(2.5, -1.0e30, 1.0e30, 0.1,
                                100, 21474836);
    g_object_set(G_OBJECT(renderer_spin1), 
                 "adjustment", adjust,
                 "climb-rate", 0.5,
                 "digits", 3, 
                 "editable", TRUE, 
                 "width", (gint)150, NULL);
    
    gtk_tree_view_column_pack_start(column, renderer_spin1, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer_spin1, "text", COLUMN_FIELD_INDEX, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_INDEX));
    g_signal_connect(G_OBJECT(column), "clicked", 
                     G_CALLBACK(column_clicked), r2_tree_view);
    
    /* Second row */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(r2_tree_view, column);
    gtk_tree_view_column_set_title(column, "Color");
    adjust = gtk_adjustment_new(0.5, 0.0, 1.0, 0.01,
                                100, 21474836);
    g_object_set(G_OBJECT(renderer_spin2), 
                 "adjustment", adjust,
                 "climb-rate", 0.5,
                 "digits", 3, 
                 "editable", TRUE, 
                 "width", (gint)150, NULL);
    
    gtk_tree_view_column_pack_start(column, renderer_spin2, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer_spin2, "text", COLUMN_FIELD_NAME, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_NAME));
    g_signal_connect(G_OBJECT(column), "clicked", 
                     G_CALLBACK(column_clicked), r2_tree_view);
    
    /* 選択モード */
    selection = gtk_tree_view_get_selection(r2_tree_view);
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* 1行毎に背景色を変更 */
    gtk_tree_view_set_rules_hint(r2_tree_view, TRUE);
    
    /* ソート */
    column = gtk_tree_view_get_column(r2_tree_view, COLUMN_FIELD_INDEX);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_INDEX, GTK_SORT_ASCENDING);
}


void add_real2_list_box_w_addbottun(GtkTreeView *r2_tree_view, 
                                         GtkWidget *button_add, GtkWidget *button_delete, 
                                         GtkWidget *vbox)
{
    GtkWidget *hbox;
    
    GtkWidget *label;
    GtkWidget *scrolled_window;
    
    GtkTreeSelection *selection;
    GtkTreeModel *model;
    GtkTreeModel *child_model;
    gulong changed_handler_id;
    GList *list;
    
    char *c_label;
    
    c_label = (char *)calloc(KCHARA_C, sizeof(char));
    
    hbox = gtk_hbox_new(FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
    
    /* Pack bottuns */
    gtk_box_pack_start(GTK_BOX(hbox), button_add, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox), button_delete, FALSE, FALSE, 0);
    
    label = gtk_label_new("");
    gtk_box_pack_end(GTK_BOX(hbox), label, TRUE, TRUE, 0);
    
    /* Delete data bottun */
    
    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(scrolled_window, 400, 300);
    gtk_container_add(GTK_CONTAINER(scrolled_window), r2_tree_view);
    gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, TRUE, TRUE, 0);
    
    /*
     * selectionにchangedシグナルハンドラを登録する。
     * 後で同じchild_modelを使用しているselectionのchangedシグナルをブロック出来るように
     * child_modelにselectionのリストを、selectionにシグナルハンドラIDを登録する。
     * changedハンドラ内で使用するlabelも同様に登録しておく。
     */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(r2_tree_view));
    changed_handler_id = g_signal_connect(G_OBJECT(selection), "changed",
                                          G_CALLBACK(set_last_field_to_label), NULL);
    g_object_set_data(G_OBJECT(selection), "changed_handler_id", GUINT_TO_POINTER(changed_handler_id));
    g_object_set_data(G_OBJECT(selection), "label", label);
    
    model = gtk_tree_view_get_model(GTK_TREE_VIEW(r2_tree_view));
    child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    list = g_object_get_data(G_OBJECT(child_model), "selection_list");
    list = g_list_append(list, selection);
    g_object_set_data(G_OBJECT(child_model), "selection_list", list);
    
};
