/*
//  tree_view_4_force_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#include "tree_view_4_force_GTK.h"


void init_dimless_views_GTK(struct mhd_model_control_c *model_ctl, struct dimless_views *dless_vws){
    dless_vws->dless_ctl_gtk = model_ctl->dless_ctl;
    return;
}

void dealloc_dimless_views_GTK(struct dimless_views *dless_vws){
    return;
}


/* Append new data at the end of list */
int append_dimless_item(int index, char *c_tbl, char *c_math, double r_data, GtkTreeModel *child_model)
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


void append_default_coefs_label(GtkWidget *label_tree){
    GtkTreeModel *model = gtk_tree_view_get_model (label_tree);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    int i, j;
    GtkTreeIter iter;
    for(i=0;i<NUM_DEFAULT_COEF_DEF;i++){
        j = append_dimless_item(i, default_coefs_def[i].flag_name,
                            default_coefs_def[i].flag_math, 
                            default_coefs_def[i].value, child_model);
    }
    
}

int append_dimless_list(int index, struct chara_real_ctl_list *head, GtkWidget *label_tree)
{
    GtkTreeModel *model = gtk_tree_view_get_model (label_tree);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    head = head->_next;
    while (head != NULL){
        index = append_dimless_item(index, head->cr_item->c_tbl, head->cr_item->c_tbl, 
                            head->cr_item->r_data, child_model);
        head = head->_next;
    };
    return index;
}
static void name_edited_cb(GtkCellRendererText *cell, gchar *path_str, gchar *new_text, gpointer user_data)
{
    struct dimless_views *dless_vws = (struct dimless_views *) user_data;
    GtkTreeModel *model = gtk_tree_view_get_model (dless_vws->dimless_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;

    gchar *old_text;
    gchar *math_string;
    double old_value, new_value;

    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &old_text, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_MATH, &math_string, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_VALUE, &old_value, -1);
    
    printf("Change %s to %s\n", old_text, new_text);

    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_NAME, new_text, -1);
    gtk_tree_path_free(child_path);  
    gtk_tree_path_free(path);  
    
    update_chara_real_ctl_list_by_c_tbl(old_text, new_text, old_value,
                                        &dless_vws->dless_ctl_gtk->dimless_list);
/*
    write_chara_real_ctl_list(stdout, 0, "dimless_test", 
                              &dless_vws->dless_ctl_gtk->dimless_list);
 */
}
static void value_edited_cb(GtkCellRendererText *cell, gchar *path_str, gchar *new_text, gpointer user_data)
{
    struct dimless_views *dless_vws = (struct dimless_views *) user_data;
    GtkTreeModel *model = gtk_tree_view_get_model (dless_vws->dimless_tree_view);  
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
    
    update_chara_real_ctl_list_by_c_tbl(old_text, old_text, new_value,
                                        &dless_vws->dless_ctl_gtk->dimless_list);
/*
    write_chara_real_ctl_list(stdout, 0, "dimless_test", 
                              &dless_vws->dless_ctl_gtk->dimless_list);
 */
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


void create_dimless_tree_view(struct dimless_views *dless_vws)
{
    /*    GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);*/
    GtkTreeModel *model;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    
    GtkListStore *child_model;

    GtkAdjustment *adjust;

    int i;
    
    /* Construct empty list storage */
    child_model = gtk_list_store_new(4, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING,
                                     G_TYPE_DOUBLE);
    g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);
    
    /* ソート用のモデルを作成してツリービューにセットする */
    model = gtk_tree_model_sort_new_with_model(child_model);
    gtk_tree_view_set_model(GTK_TREE_VIEW(dless_vws->dimless_tree_view), model);
    
    /* First raw */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(GTK_TREE_VIEW(dless_vws->dimless_tree_view), column);
    gtk_tree_view_column_set_title(column, "Index");
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(column, renderer, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_FIELD_INDEX, NULL);
    g_object_set(renderer, "width", (gint)60, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_INDEX));
    g_signal_connect(G_OBJECT(column), "clicked", 
                     G_CALLBACK(column_clicked), dless_vws->dimless_tree_view);
    
    /* Second row */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(GTK_TREE_VIEW(dless_vws->dimless_tree_view), column);
    gtk_tree_view_column_set_title(column, "Field name");
    renderer = gtk_cell_renderer_text_new();
    g_object_set(G_OBJECT(renderer), "editable", TRUE, NULL);
    g_signal_connect(G_OBJECT(renderer), "edited", G_CALLBACK(name_edited_cb), dless_vws);
    gtk_tree_view_column_pack_start(column, renderer, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_FIELD_NAME, NULL);
    g_object_set(renderer, "width", (gint)150, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_NAME));
    g_signal_connect(G_OBJECT(column), "clicked", 
                     G_CALLBACK(column_clicked), dless_vws->dimless_tree_view);
    
    /* Third row */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(GTK_TREE_VIEW(dless_vws->dimless_tree_view), column);
    gtk_tree_view_column_set_title(column, "value");
    adjust = gtk_adjustment_new(2.5, -1.0e30, 1.0e30, 0.1,
                                100, 21474836);
    renderer = gtk_cell_renderer_spin_new();
    g_object_set(G_OBJECT(renderer), 
                 "adjustment", adjust,
                 "climb-rate", 0.5,
                 "digits", 3, 
                 "editable", TRUE, 
                 "width", (gint)150, NULL);

    g_signal_connect(G_OBJECT(renderer), "edited", G_CALLBACK(value_edited_cb), dless_vws);
    gtk_tree_view_column_pack_start(column, renderer, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_FIELD_VALUE, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_VALUE));
    g_signal_connect(G_OBJECT(column), "clicked", 
                     G_CALLBACK(column_clicked), dless_vws->dimless_tree_view);
    
    
    /* 選択モード */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(dless_vws->dimless_tree_view));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* 1行毎に背景色を変更 */
    gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(dless_vws->dimless_tree_view), TRUE);
    
    /* ソート */
    column = gtk_tree_view_get_column(GTK_TREE_VIEW(dless_vws->dimless_tree_view), COLUMN_FIELD_INDEX);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_INDEX, GTK_SORT_ASCENDING);
}

void init_dimless_tree_view(struct dimless_views *dless_vws){
    create_dimless_tree_view(dless_vws);
    dless_vws->index_dless = append_dimless_list(dless_vws->index_dless, 
                                                 &dless_vws->dless_ctl_gtk->dimless_list, dless_vws->dimless_tree_view);
}

void create_used_dimless_tree_views(struct dimless_views *dless_vws)
{
    create_fixed_constant_tree(dless_vws->default_dless_view);
    append_default_coefs_label(dless_vws->default_dless_view);
}

