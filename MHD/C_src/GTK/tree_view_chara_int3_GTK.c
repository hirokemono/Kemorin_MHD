/*
//  tree_view_chara_int3_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/21.
*/

#include "tree_view_chara_int3_GTK.h"

/* Append new data at the end of list */
int append_ci3_item_to_tree(int index, char *c_tbl, int i1_data, int i2_data, int i3_data, 
			GtkTreeModel *child_model)
{
    GtkTreeIter iter;
    
    gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_INDEX, index,
                       COLUMN_FIELD_NAME,  c_tbl,
                       COLUMN_FIELD_MATH,  i1_data,
                       COLUMN_FORTH, i2_data,
                       COLUMN_FIFTH,  i3_data,
                       -1);
    return index + 1;
}

int append_ci3_list_from_ctl(int index, struct chara_int3_ctl_list *head, 
			GtkTreeView *ci3_tree_view)
{
    GtkTreeModel *model = gtk_tree_view_get_model (ci3_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    head = head->_next;
    while (head != NULL){
        index = append_ci3_item_to_tree(index, head->ci3_item->c_tbl,  
					head->ci3_item->i_data[0], head->ci3_item->i_data[1], 
					head->ci3_item->i_data[2], child_model);
        head = head->_next;
    };
    return index;
}


void ci3_tree_name_edited(gchar *path_str, gchar *new_text,
			GtkTreeView *ci3_tree_view, struct chara_int3_clist *ci3_clist)
{
    GtkTreeModel *model = gtk_tree_view_get_model (ci3_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;

    gchar *old_text;
    int old_value1, old_value2, old_value3;

    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME,  &old_text, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_MATH,  &old_value1, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FORTH, &old_value2, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIFTH,  &old_value3, -1);
    
    printf("Change %s to %s\n", old_text, new_text);

    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_NAME, new_text, -1);
    gtk_tree_path_free(child_path);  
    gtk_tree_path_free(path);  
    
	update_chara_int3_clist_by_c_tbl(old_text, new_text, 
				old_value1, old_value2, old_value3, ci3_clist);
}

void ci3_tree_value1_edited(gchar *path_str, gchar *new_text, 
			GtkTreeView *ci3_tree_view, struct chara_int3_clist *ci3_clist)
{
    GtkTreeModel *model = gtk_tree_view_get_model (ci3_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;
    
    gchar *old_text;
    int old_value1, old_value2, old_value3;
    int new_value;
    
    sscanf(new_text, "%d", &new_value);
    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME,  &old_text, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_MATH,  &old_value1, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FORTH, &old_value2, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIFTH,  &old_value3, -1);
    
    printf("Change %d to %d\n", old_value1, new_value);
    
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_MATH, new_value, -1);
    gtk_tree_path_free(child_path);  
    gtk_tree_path_free(path);  
    
	update_chara_int3_clist_by_c_tbl(old_text, new_text, 
				new_value, old_value2, old_value3, ci3_clist);
 
}

void ci3_tree_value2_edited(gchar *path_str, gchar *new_text, 
			GtkTreeView *ci3_tree_view, struct chara_int3_clist *ci3_clist)
{
    GtkTreeModel *model = gtk_tree_view_get_model (ci3_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;
    
    gchar *old_text;
    int old_value1, old_value2, old_value3;
    int new_value;
    
    sscanf(new_text, "%d", &new_value);
    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME,  &old_text, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_MATH,  &old_value1, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FORTH, &old_value2, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIFTH,  &old_value3, -1);
    
    printf("Change %d to %d\n", old_value2, new_value);
    
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FORTH, new_value, -1);
    gtk_tree_path_free(child_path);  
    gtk_tree_path_free(path);  
    
	update_chara_int3_clist_by_c_tbl(old_text, new_text, 
				old_value1, new_value, old_value3, ci3_clist);
 
}

void ci3_tree_value3_edited(gchar *path_str, gchar *new_text, 
			GtkTreeView *ci3_tree_view, struct chara_int3_clist *ci3_clist)
{
    GtkTreeModel *model = gtk_tree_view_get_model (ci3_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;
    
    gchar *old_text;
    int old_value1, old_value2, old_value3;
    int new_value;
    
    sscanf(new_text, "%d", &new_value);
    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME,  &old_text, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_MATH,  &old_value1, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FORTH, &old_value2, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIFTH,  &old_value3, -1);
    
    printf("Change %d to %d\n", old_value3, new_value);
    
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIFTH, new_value, -1);
    gtk_tree_path_free(child_path);  
    gtk_tree_path_free(path);  
    
	update_chara_int3_clist_by_c_tbl(old_text, new_text, 
				old_value1, old_value2, new_value, ci3_clist);
 
}

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

int add_ci3_list_by_bottun_GTK(int index, GtkTreeView *tree_view_to_add, 
                                     struct chara_int3_clist *ci3_clist)
{
    GtkTreeModel *model_to_add = gtk_tree_view_get_model(tree_view_to_add);
    GtkTreeModel *child_model_to_add = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_add));
        
    gchar row_string[30] = "new_number";
    int ivalue = 0;
    
	index = append_ci3_item_to_tree(index, row_string, ivalue, ivalue, ivalue, 
				child_model_to_add);
	append_chara_int3_clist(row_string, ivalue, ivalue, ivalue, ci3_clist);
	
    return index;
}

int add_ci3_list_from_combobox_GTK(int index, GtkTreePath *path, GtkTreeModel *tree_model,
			GtkTreeView *tree_view_to_add, struct chara_int3_clist *ci3_clist)
{
    GtkTreeModel *model_to_add = gtk_tree_view_get_model(tree_view_to_add);
    GtkTreeModel *child_model_to_add = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_add));
      
    GtkTreeIter iter;
    
    gchar *row_string;
    int ivalue1, ivalue2, ivalue3;
    
    gtk_tree_model_get_iter(tree_model, &iter, path);  
    gtk_tree_model_get(tree_model, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(tree_model, &iter, COLUMN_FIELD_MATH, &ivalue1, -1);
    gtk_tree_model_get(tree_model, &iter, COLUMN_FORTH, &ivalue2, -1);
    gtk_tree_model_get(tree_model, &iter, COLUMN_FIFTH, &ivalue3, -1);
    
	index = append_ci3_item_to_tree(index, row_string, ivalue1, ivalue2, ivalue3, 
				child_model_to_add);
    append_chara_int3_clist(row_string, ivalue1, ivalue2, ivalue3, ci3_clist);
    return index;
}

int add_ci3_list_from_combobox_GTK_w_one(int index, GtkTreePath *path, GtkTreeModel *tree_model,
			GtkTreeView *tree_view_to_add, struct chara_int3_clist *ci3_clist)
{
    GtkTreeModel *model_to_add = gtk_tree_view_get_model(tree_view_to_add);
    GtkTreeModel *child_model_to_add = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_add));
      
    GtkTreeIter iter;
    
    gchar *row_string;
    int ivalue = 1;
    
    gtk_tree_model_get_iter(tree_model, &iter, path);  
    gtk_tree_model_get(tree_model, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    
    index = append_ci3_item_to_tree(index, row_string, ivalue, ivalue, ivalue, child_model_to_add);
    append_chara_int3_clist(row_string, ivalue, ivalue, ivalue, ci3_clist);
    return index;
}

int add_ci3_list_items_GTK(GtkTreeView *tree_view_to_add,
			struct chara_int3_clist *ci3_clist)
{
    GtkTreeModel *model_to_add;
    GtkTreeModel *child_model_to_add;
    GtkTreeSelection *selection;
    GList *list;
    GList *reference_list;
    GList *cur;
    
    gchar *field_name;
    int ivalue;
	int index = 0;
    
    /* Get path of selected raw */
    /* The path is for tree_model_sort */
    model_to_add = gtk_tree_view_get_model(tree_view_to_add);
    child_model_to_add = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_add));
        
    selection = gtk_tree_view_get_selection(tree_view_to_add);
    list = gtk_tree_selection_get_selected_rows(selection, NULL);
    
    /* Make reference from path */
    /* After deleting data, obtained path would not be valied */
    reference_list = NULL;
    for (cur = g_list_first(list); cur != NULL; cur = g_list_next(cur)) {
        GtkTreePath *child_path;
        GtkTreeRowReference *child_reference;
        /* Convert tree model sort path into tree model path */
        child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model_to_add), 
                                                                    (GtkTreePath *)cur->data);
        
        child_reference = gtk_tree_row_reference_new(child_model_to_add, child_path);
        reference_list = g_list_append(reference_list, child_reference);
        
        gtk_tree_path_free(child_path);
        gtk_tree_path_free((GtkTreePath *)cur->data);
    }
    g_list_free(list);
    
    /* Temporary block the changed signal of GtkTreeSelection */
    block_changed_signal(G_OBJECT(child_model_to_add));
    
    /* Return reference into path and delete reference */
	
	GtkTreePath *tree_path;
	GtkTreeIter iter;
	cur = g_list_first(reference_list);
	tree_path = gtk_tree_row_reference_get_path((GtkTreeRowReference *)cur->data);
	gtk_tree_model_get_iter(child_model_to_add, &iter, tree_path);
	gtk_tree_model_get(child_model_to_add, &iter, COLUMN_FIELD_NAME, &field_name, -1);
    for (cur = g_list_first(reference_list); cur != NULL; cur = g_list_next(cur)) {
        
        /* Add */
		gchar row_string[30] = "new_number";
		ivalue = 0;
		add_chara_int3_clist_before_c_tbl(field_name, row_string, 
					ivalue, ivalue, ivalue, ci3_clist);
		
        gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
		
	}
	
	gtk_tree_path_free(tree_path);
    g_list_free(reference_list);
	
	gtk_list_store_clear(GTK_LIST_STORE(child_model_to_add));
	index = append_ci3_list_from_ctl(index, &ci3_clist->ci3_item_head, tree_view_to_add);
    /* Release the block of changed signal */
	unblock_changed_signal(G_OBJECT(child_model_to_add));
	return index;
}

void delete_ci3_list_items_GTK(GtkTreeView *tree_view_to_del,
			struct chara_int3_clist *ci3_clist)
{
    GtkTreeModel *model_to_del;
    GtkTreeModel *child_model_to_del;
    GtkTreeSelection *selection;
    GList *list;
    GList *reference_list;
    GList *cur;
    
    gchar *field_name;
    int index_field;
    int ivalue;
    
    /* Get path of selected raw */
    /* The path is for tree_model_sort */
    model_to_del = gtk_tree_view_get_model(tree_view_to_del);
    child_model_to_del = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_del));
        
    selection = gtk_tree_view_get_selection(tree_view_to_del);
    list = gtk_tree_selection_get_selected_rows(selection, NULL);
    
    /* Make reference from path */
    /* After deleting data, obtained path would not be valied */
    reference_list = NULL;
    for (cur = g_list_first(list); cur != NULL; cur = g_list_next(cur)) {
        GtkTreePath *child_path;
        GtkTreeRowReference *child_reference;
        /* Convert tree model sort path into tree model path */
        child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model_to_del), 
                                                                    (GtkTreePath *)cur->data);
        
        child_reference = gtk_tree_row_reference_new(child_model_to_del, child_path);
        reference_list = g_list_append(reference_list, child_reference);
        
        gtk_tree_path_free(child_path);
        gtk_tree_path_free((GtkTreePath *)cur->data);
    }
    g_list_free(list);
    
    /* Temporary block the changed signal of GtkTreeSelection */
    block_changed_signal(G_OBJECT(child_model_to_del));
    
    /* Return reference into path and delete reference */
    for (cur = g_list_first(reference_list); cur != NULL; cur = g_list_next(cur)) {
        GtkTreePath *tree_path;
        GtkTreeIter iter;
        tree_path = gtk_tree_row_reference_get_path((GtkTreeRowReference *)cur->data);
        gtk_tree_model_get_iter(child_model_to_del, &iter, tree_path);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_NAME, &field_name, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_MATH, &ivalue, -1);
        
        printf("To be moved: %d, %s %d\n", index_field, field_name, ivalue);
        /* Delete */
        gtk_list_store_remove(GTK_LIST_STORE(child_model_to_del), &iter);

        gtk_tree_path_free(tree_path);
        gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
        
        /* Update control data */
        del_chara_int3_clist_by_c_tbl(field_name, ci3_clist);
    }
    g_list_free(reference_list);
    
    /* Release the block of changed signal */
    unblock_changed_signal(G_OBJECT(child_model_to_del));
}


void create_text_int3_tree_view(struct chara_int3_clist *ci3_clist, GtkTreeView *ci3_tree_view,
			GtkCellRenderer *renderer_text, GtkCellRenderer *renderer_spin)
{
    /*    GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);*/
	
    GtkTreeModel *model;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    
    GtkListStore *child_model;

    GtkAdjustment *adjust;

    
	/* Construct empty list storage */
    child_model = gtk_list_store_new(5, G_TYPE_INT, G_TYPE_STRING,
                                     G_TYPE_INT, G_TYPE_INT, G_TYPE_INT);
    g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);
    
    /* Construct model for sorting and set to tree view */
    model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    gtk_tree_view_set_model(GTK_TREE_VIEW(ci3_tree_view), model);
    
    /* First raw */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(ci3_tree_view, column);
    gtk_tree_view_column_set_title(column, "Index");
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(column, renderer, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_FIELD_INDEX, NULL);
    g_object_set(renderer, "width", (gint)60, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_INDEX));
    g_signal_connect(G_OBJECT(column), "clicked", 
                     G_CALLBACK(column_clicked), (gpointer) ci3_tree_view);
    
    /* Second row */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(ci3_tree_view, column);
    gtk_tree_view_column_set_title(column, ci3_clist->c1_name);
    g_object_set(G_OBJECT(renderer_text), "editable", TRUE, NULL);
    gtk_tree_view_column_pack_start(column, renderer_text, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer_text, "text", COLUMN_FIELD_NAME, NULL);
    g_object_set(renderer_text, "width", (gint)150, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_NAME));
    g_signal_connect(G_OBJECT(column), "clicked", 
                     G_CALLBACK(column_clicked), (gpointer) ci3_tree_view);
    
    /* Third row */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(ci3_tree_view, column);
    gtk_tree_view_column_set_title(column, ci3_clist->i1_name);
    adjust = gtk_adjustment_new(10, -32768, 32768, 1,
                                100, 21474836);
    g_object_set(G_OBJECT(renderer_spin), 
                 "adjustment", adjust,
                 "climb-rate", 1,
                 "digits", 0, 
                 "editable", TRUE, 
                 "width", (gint)150, NULL);

    gtk_tree_view_column_pack_start(column, renderer_spin, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer_spin, "text", COLUMN_FIELD_MATH, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_MATH));
    g_signal_connect(G_OBJECT(column), "clicked", 
                     G_CALLBACK(column_clicked), (gpointer) ci3_tree_view);
    
    /* Forth row */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(ci3_tree_view, column);
    gtk_tree_view_column_set_title(column, ci3_clist->i2_name);
    adjust = gtk_adjustment_new(10, -32768, 32768, 1,
                                100, 21474836);
    g_object_set(G_OBJECT(renderer_spin), 
                 "adjustment", adjust,
                 "climb-rate", 1,
                 "digits", 0, 
                 "editable", TRUE, 
                 "width", (gint)150, NULL);

    gtk_tree_view_column_pack_start(column, renderer_spin, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer_spin, "text", COLUMN_FORTH, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FORTH));
    g_signal_connect(G_OBJECT(column), "clicked", 
                     G_CALLBACK(column_clicked), (gpointer) ci3_tree_view);
    
    /* Fifth row */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(ci3_tree_view, column);
    gtk_tree_view_column_set_title(column, ci3_clist->i2_name);
    adjust = gtk_adjustment_new(10, -32768, 32768, 1,
                                100, 21474836);
    g_object_set(G_OBJECT(renderer_spin), 
                 "adjustment", adjust,
                 "climb-rate", 1,
                 "digits", 0, 
                 "editable", TRUE, 
                 "width", (gint)150, NULL);

    gtk_tree_view_column_pack_start(column, renderer_spin, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer_spin, "text", COLUMN_FIFTH, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIFTH));
    g_signal_connect(G_OBJECT(column), "clicked", 
                     G_CALLBACK(column_clicked), (gpointer) ci3_tree_view);
    
    /* 選択モード */
    selection = gtk_tree_view_get_selection(ci3_tree_view);
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
    column = gtk_tree_view_get_column(ci3_tree_view, COLUMN_FIELD_INDEX);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_INDEX, GTK_SORT_ASCENDING);
}


void add_chara_int3_list_box_w_addbottun(GtkTreeView *ci3_tree_view, 
			GtkWidget *button_add, GtkWidget *button_delete, 
			GtkWidget *vbox)
{
    GtkWidget *hbox;
    GtkWidget *scrolled_window;
    
    char *c_label;
    
    c_label = (char *)calloc(KCHARA_C, sizeof(char));
    
    hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
    
    /* Pack bottuns */
    gtk_box_pack_start(GTK_BOX(hbox), button_add, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox), button_delete, FALSE, FALSE, 0);

    /* Delete data bottun */
    
    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(scrolled_window, 400, 300);
    gtk_container_add(GTK_CONTAINER(scrolled_window), GTK_WIDGET(ci3_tree_view));
    gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, TRUE, TRUE, 0);
    
    add_sorting_signal_w_label(ci3_tree_view, hbox);
};

void add_chara_int3_list_box_w_combobox(GtkTreeView *ci3_tree_view, 
			GtkWidget *button_add, GtkWidget *combobox_add, GtkWidget *button_delete, 
			GtkWidget *vbox)
{
    GtkWidget *hbox;
    GtkCellRenderer *column_add;
    GtkWidget *scrolled_window;
    
    char *c_label;
    
    c_label = (char *)calloc(KCHARA_C, sizeof(char));
    
    hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
    
    /* Pack bottuns */
    gtk_box_pack_start(GTK_BOX(hbox), button_add, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox), combobox_add, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox), button_delete, FALSE, FALSE, 0);

    /* Delete data bottun */
    
    column_add = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_add), column_add, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_add), column_add,
                                   "text", COLUMN_FIELD_NAME, NULL);
	
	
    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(scrolled_window, 400, 300);
    gtk_container_add(GTK_CONTAINER(scrolled_window), GTK_WIDGET(ci3_tree_view));
    gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, TRUE, TRUE, 0);
    
    add_sorting_signal_w_label(ci3_tree_view, hbox);
};
