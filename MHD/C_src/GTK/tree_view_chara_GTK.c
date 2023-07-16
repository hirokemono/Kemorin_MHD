/*
//  tree_view_chara_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/21.
*/

#include "tree_view_chara_GTK.h"

/* Append new data at the end of list */
int append_c_item_to_tree(int index, const char *c_tbl, GtkTreeModel *child_model){
    GtkTreeIter iter;
    
    gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_INDEX, index,
                       COLUMN_FIELD_NAME,  c_tbl,
                       -1);
    return index + 1;
}

int append_c_list_from_ctl(int index, struct chara_ctl_list *head, 
			GtkTreeView *c_tree_view)
{
    GtkTreeModel *model = gtk_tree_view_get_model (c_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    head = head->_next;
    while (head != NULL){
        index = append_c_item_to_tree(index, head->c_item->c_tbl, child_model);
        head = head->_next;
    };
    return index;
}

int append_c_list_from_array(int index, int num, char **c_tbl,
                             GtkTreeView *c_tree_view)
{
    int i;
    GtkTreeModel *model = gtk_tree_view_get_model(c_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    for(i=0;i<num;i++){
        index = append_c_item_to_tree(index, c_tbl[i], child_model);
    };
    return index;
}


void c_tree_name_edited(gchar *path_str, gchar *new_text,
			GtkTreeView *c_tree_view, struct chara_clist *c_clist)
{
    GtkTreeModel *model = gtk_tree_view_get_model (c_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;

    gchar *old_text;

    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &old_text, -1);
    
    printf("Change %s to %s\n", old_text, new_text);

    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_NAME, new_text, -1);
    gtk_tree_path_free(child_path);  
    gtk_tree_path_free(path);  
    
    update_chara_clist_by_c_tbl(old_text, new_text, c_clist);
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

int add_c_list_by_bottun_GTK(int index, GtkTreeView *tree_view_to_add, 
                                     struct chara_clist *c_clist)
{
    GtkTreeModel *model_to_add = gtk_tree_view_get_model(tree_view_to_add);
    GtkTreeModel *child_model_to_add = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_add));
        
    gchar row_string[30] = "new";
    
    index = append_c_item_to_tree(index, row_string, child_model_to_add);
	append_chara_clist(row_string, c_clist);
	
    return index;
}

int add_c_list_from_combobox_GTK(int index, GtkTreePath *path, GtkTreeModel *tree_model,
			GtkTreeView *tree_view_to_add, struct chara_clist *c_clist)
{
    GtkTreeModel *model_to_add = gtk_tree_view_get_model(tree_view_to_add);
    GtkTreeModel *child_model_to_add = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_add));
      
    GtkTreeIter iter;
    
    gchar *row_string;
    
    gtk_tree_model_get_iter(tree_model, &iter, path);  
    gtk_tree_model_get(tree_model, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    
    index = append_c_item_to_tree(index, row_string, child_model_to_add);
    append_chara_clist(row_string, c_clist);
    return index;
}

int add_c_list_from_combobox_GTK_w_one(int index, GtkTreePath *path, GtkTreeModel *tree_model,
			GtkTreeView *tree_view_to_add, struct chara_clist *c_clist)
{
    GtkTreeModel *model_to_add = gtk_tree_view_get_model(tree_view_to_add);
    GtkTreeModel *child_model_to_add = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_add));
      
    GtkTreeIter iter;
    
    gchar *row_string;
    
    gtk_tree_model_get_iter(tree_model, &iter, path);  
    gtk_tree_model_get(tree_model, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    
    index = append_c_item_to_tree(index, row_string, child_model_to_add);
    append_chara_clist(row_string, c_clist);
    return index;
}

int add_c_list_items_GTK(GtkTreeView *tree_view_to_add, struct chara_clist *c_clist)
{
    GtkTreeModel *model_to_add;
    GtkTreeModel *child_model_to_add;
    GtkTreeSelection *selection;
    GList *list;
    GList *reference_list;
    GList *cur;
    
    gchar *old_strng;
	int index = 0;
    
   	if(count_chara_clist(c_clist) == 0){
		append_chara_clist("  ", c_clist);
		index = count_chara_clist(c_clist);
        index = append_c_list_from_ctl(index, &c_clist->c_item_head, tree_view_to_add);
        return index;
    };
	
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
	
	gchar row_string[30] = "new_number";
	GtkTreeIter iter;
	cur = g_list_first(reference_list);
	if(cur == NULL){
		append_chara_clist(row_string, c_clist);
		index = count_chara_clist(c_clist);
	} else {
		GtkTreePath *tree_path = gtk_tree_row_reference_get_path((GtkTreeRowReference *)cur->data);
		gtk_tree_model_get_iter(child_model_to_add, &iter, tree_path);
		gtk_tree_model_get(child_model_to_add, &iter, COLUMN_FIELD_NAME, &old_strng, -1);
		for (cur = g_list_first(reference_list); cur != NULL; cur = g_list_next(cur)) {
			/* Add */
			add_chara_clist_before_c_tbl(old_strng, row_string, c_clist);
			gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
		};
		gtk_tree_path_free(tree_path);
	};
    g_list_free(reference_list);
	
	gtk_list_store_clear(GTK_LIST_STORE(child_model_to_add));
	index = append_c_list_from_ctl(index, &c_clist->c_item_head, tree_view_to_add);
    /* Release the block of changed signal */
	unblock_changed_signal(G_OBJECT(child_model_to_add));
	return index;
}

void delete_c_list_items_GTK(GtkTreeView *tree_view_to_del, struct chara_clist *c_clist)
{
    GtkTreeModel *model_to_del;
    GtkTreeModel *child_model_to_del;
    GtkTreeSelection *selection;
    GList *list;
    GList *reference_list;
    GList *cur;
    
    gchar *old_strng;
    
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
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_NAME, &old_strng, -1);
        
/*        printf("To be moved: %s\n", old_strng); */
        /* Delete */
        gtk_list_store_remove(GTK_LIST_STORE(child_model_to_del), &iter);

        gtk_tree_path_free(tree_path);
        gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
        
        /* Update control data */
        del_chara_clist_by_c_tbl(old_strng, c_clist);
    }
    g_list_free(reference_list);
    
    /* Release the block of changed signal */
    unblock_changed_signal(G_OBJECT(child_model_to_del));
}

void create_text_tree_view(GtkTreeView *c_tree_view, GtkCellRenderer *renderer_text)
{
    /*    GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);*/
	
    GtkTreeModel *model;
    GtkTreeViewColumn *column;
    GtkTreeViewColumn *column_1st;
    GtkTreeSelection *selection;
    
    GtkListStore *child_model;

	/* Construct empty list storage */
    child_model = gtk_list_store_new(2, G_TYPE_INT, G_TYPE_STRING);
    g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);
    
    /* Construct model for sorting and set to tree view */
    model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    gtk_tree_view_set_model(GTK_TREE_VIEW(c_tree_view), model);
    
    /* First raw */
	column_1st = gtk_tree_view_column_new();
    gtk_tree_view_append_column(c_tree_view, column_1st);
    gtk_tree_view_column_set_title(column_1st, "Field name");
    gtk_tree_view_column_set_resizable(column_1st, TRUE);
    gtk_tree_view_column_set_clickable(column_1st, TRUE);
    g_object_set_data(G_OBJECT(column_1st), "column_id", GINT_TO_POINTER(COLUMN_FIELD_NAME));
	/*
    g_signal_connect(G_OBJECT(column_1st), "clicked", 
					 G_CALLBACK(column_clicked), (gpointer) c_tree_view);
	*/
	
    gtk_tree_view_column_pack_start(column_1st, renderer_text, TRUE);
    gtk_tree_view_column_set_attributes(column_1st, renderer_text, "text", COLUMN_FIELD_NAME, NULL);
	g_object_set(renderer_text, "width", (gint)120, NULL);
    g_object_set(G_OBJECT(renderer_text), "editable", TRUE, NULL);
	
    /* 選択モード */
    selection = gtk_tree_view_get_selection(c_tree_view);
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
    column = gtk_tree_view_get_column(c_tree_view, COLUMN_FIELD_NAME);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_NAME, GTK_SORT_ASCENDING);
}


GtkWidget *chara_list_box_expander(char *array_name_c, GtkTreeView *c_tree_view, 
			GtkWidget *button_add, GtkWidget *button_delete)
{
	GtkWidget *expander;
    
    GtkWidget *hbox_1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	
    /* Pack bottuns */
    gtk_box_pack_start(GTK_BOX(hbox_1), button_add, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1), button_delete, FALSE, FALSE, 0);
	
	GtkWidget *label_1 = gtk_label_new("");
    gtk_box_pack_end(GTK_BOX(hbox_1), label_1, TRUE, TRUE, 0);
	
	GtkWidget *vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_1), hbox_1, FALSE, FALSE, 0);
	
    GtkWidget *scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(scrolled_window, 150, 300);
    gtk_container_add(GTK_CONTAINER(scrolled_window), GTK_WIDGET(c_tree_view));
    gtk_box_pack_start(GTK_BOX(vbox_1), scrolled_window, TRUE, TRUE, 0);
	
	GtkWidget *Frame_1 = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_1), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame_1), vbox_1);
	
	expander = gtk_expander_new_with_mnemonic(duplicate_underscore(array_name_c));
	gtk_container_add(GTK_CONTAINER(expander), Frame_1);
	
	return expander;
};
