/*
//  tree_view_int_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "tree_view_int_GTK.h"


/* Append new data at the end of list */
static int append_int_item_to_tree(const int index, const int i1_data, 
                                   GtkTreeModel *child_model){
    GtkTreeIter iter;
    
    gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_INDEX, i1_data,
                       -1);
    return index + 1;
}

int append_int_list_from_ctl(int index, struct int_ctl_list *head, 
							 GtkTreeView *int_tree_view){
    GtkTreeModel *model = gtk_tree_view_get_model (int_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    head = head->_next;
	while (head != NULL){
		index = append_int_item_to_tree(index, head->i_item->i_data, child_model);
        head = head->_next;
    };
    return index;
}


void int_tree_value1_edited(gchar *path_str, gchar *new_text,
			 GtkTreeView *int_tree_view, struct int_clist *i_clist_gtk){
	GtkTreeModel *model = gtk_tree_view_get_model (int_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;
    
	int old_int1, new_int;
    
    if(sscanf(new_text, "%d", &new_int) < 1) return;
    gtk_tree_model_get_iter(child_model, &iter, child_path);  
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &old_int1, -1);
    
    printf("Change %d to %d\n", old_int1, new_int);
    
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_INDEX, new_int, -1);
    gtk_tree_path_free(child_path);
    gtk_tree_path_free(path);
    
	update_int_clist_by_c_tbl(old_int1, new_int, i_clist_gtk);
};

static void column_clicked(GtkTreeViewColumn *column, gpointer user_data){
    GtkTreeView *int_tree_view = GTK_TREE_VIEW(user_data);
    GtkTreeModel *model;
    gint column_id;
    gint cur_id;
    GtkSortType order;
    GtkTreeViewColumn *cur_column;
        
    if (gtk_widget_is_focus(GTK_WIDGET(int_tree_view)) == FALSE) {
        gtk_widget_grab_focus(GTK_WIDGET(int_tree_view));
    }
    
    column_id = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(column), "column_id"));
    model = gtk_tree_view_get_model(int_tree_view);
    
    /* 現在のソート列と同じときは昇順／降順を反転する、違うときはクリックした列で昇順ソートする */
    if (gtk_tree_sortable_get_sort_column_id(GTK_TREE_SORTABLE(model), &cur_id, &order) == TRUE) {
        if (cur_id == column_id) {
            order = (order == GTK_SORT_ASCENDING) ? GTK_SORT_DESCENDING : GTK_SORT_ASCENDING;
        } else {
            order = GTK_SORT_ASCENDING;
        }
        cur_column = gtk_tree_view_get_column(int_tree_view, cur_id);
        gtk_tree_view_column_set_sort_indicator(cur_column, FALSE);
    } else {
        order = GTK_SORT_ASCENDING;
    }
    gtk_tree_view_column_set_sort_order(column, order);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), column_id, order);
}

int add_int_list_items(GtkTreeView *int_tree_view, struct int_clist *i_clist_gtk){
	GtkTreeModel *model_to_add;
    GtkTreeModel *child_model_to_add;
    GtkTreeSelection *selection;
    GList *list;
    GList *reference_list;
    GList *cur;
    int   i1value = 0;
	int index = 0;
    
   	if(count_int_clist(i_clist_gtk) == 0){
		append_int_clist(i1value, i_clist_gtk);
		index = count_int_clist(i_clist_gtk);
        index = append_int_list_from_ctl(index, &i_clist_gtk->i_item_head, int_tree_view);
        return index;
    };
	
    /* Get path of selected raw */
    /* The path is for tree_model_sort */
    model_to_add = gtk_tree_view_get_model(int_tree_view);
    child_model_to_add = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_add));
        
    selection = gtk_tree_view_get_selection(int_tree_view);
    list = gtk_tree_selection_get_selected_rows(selection, NULL);
    
	if(g_list_first(list) == NULL) return index;
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
	
	GtkTreeIter iter;
	cur = g_list_first(reference_list);
	if(cur == NULL){
		append_int_clist(i1value, i_clist_gtk);
		index = count_int_clist(i_clist_gtk);
	} else {
		GtkTreePath *tree_path = gtk_tree_row_reference_get_path((GtkTreeRowReference *)cur->data);
		gtk_tree_model_get_iter(child_model_to_add, &iter, tree_path);
		gtk_tree_model_get(child_model_to_add, &iter, COLUMN_FIELD_INDEX, &i1value, -1);
		for (cur = g_list_first(reference_list); cur != NULL; cur = g_list_next(cur)) {
			/* Add */
			add_int_clist_before_c_tbl(i1value, i1value, i_clist_gtk);
			
			gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
		}
		gtk_tree_path_free(tree_path);
	};
	
    g_list_free(reference_list);
	
	gtk_list_store_clear(GTK_LIST_STORE(child_model_to_add));
	index = append_int_list_from_ctl(index, &i_clist_gtk->i_item_head, int_tree_view);
	return index;
}

void delete_int_list_items(GtkTreeView *int_tree_view, struct int_clist *i_clist_gtk)
{
    GtkTreeModel *model_to_del;
    GtkTreeModel *child_model_to_del;
    GtkTreeSelection *selection;
    GList *list;
    GList *reference_list;
    GList *cur;
	int i1value;
    
    /* Get path of selected raw */
    /* The path is for tree_model_sort */
    model_to_del = gtk_tree_view_get_model(int_tree_view);
    child_model_to_del = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_del));
    
    selection = gtk_tree_view_get_selection(int_tree_view);
	list = gtk_tree_selection_get_selected_rows(selection, NULL);
	if(g_list_first(list) == NULL) return;
    
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
    
	
	/* Return reference into path and delete reference */
    for (cur = g_list_first(reference_list); cur != NULL; cur = g_list_next(cur)) {
        GtkTreePath *tree_path;
        GtkTreeIter iter;
        tree_path = gtk_tree_row_reference_get_path((GtkTreeRowReference *)cur->data);
        gtk_tree_model_get_iter(child_model_to_del, &iter, tree_path);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_INDEX, &i1value, -1);
        
        /* Delete */
        gtk_list_store_remove(GTK_LIST_STORE(child_model_to_del), &iter);
        
        gtk_tree_path_free(tree_path);
        gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
        
        /* Update control data */
        del_int_clist_by_c_tbl(i1value, i_clist_gtk);
    }
    g_list_free(reference_list);
}

void create_int_tree_view(GtkTreeView *int_tree_view, struct int_clist *i_clist_gtk, 
						  GtkCellRenderer *renderer_value1){
    /*    GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);*/
    
    GtkTreeModel *model;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    
    GtkListStore *child_model;
    /* GtkAdjustment *adjust; */
    
    /* Construct empty list storage */
    child_model = gtk_list_store_new(1, G_TYPE_INT);
    g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);
    
    /* Construct model for sorting and set to tree view */
    model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    gtk_tree_view_set_model(GTK_TREE_VIEW(int_tree_view), model);
    
    /* First raw */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(int_tree_view, column);
    gtk_tree_view_column_set_title(column, i_clist_gtk->i1_name);
    g_object_set(G_OBJECT(renderer_value1), "editable", TRUE, NULL);
    g_object_set(renderer_value1, "width", (gint)70, NULL);
    gtk_tree_view_column_pack_start(column, renderer_value1, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer_value1, "text", COLUMN_FIELD_INDEX, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
	g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_INDEX));
	/*
    g_signal_connect(G_OBJECT(column), "clicked", 
                     G_CALLBACK(column_clicked), (gpointer) int_tree_view);
    */
	/* 選択モード */
    selection = gtk_tree_view_get_selection(int_tree_view);
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
    column = gtk_tree_view_get_column(int_tree_view, COLUMN_FIELD_INDEX);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_INDEX, GTK_SORT_ASCENDING);
}

GtkWidget * int_list_box_expander(char *array_name_c, GtkWidget *int_tree_view, 
								  GtkWidget *button_add, GtkWidget *button_delete){
	GtkWidget *expander;
    
    GtkWidget *scrolled_window;
    GtkTreeSelection *selection;
    GtkTreeModel *model;
    GtkTreeModel *child_model;
    GList *list;
    
	GtkWidget *hbox_1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
    
    /* Pack bottuns */
    gtk_box_pack_start(GTK_BOX(hbox_1), button_add, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1), button_delete, FALSE, FALSE, 0);
	
	GtkWidget *label_1 = gtk_label_new("");
    gtk_box_pack_end(GTK_BOX(hbox_1), label_1, TRUE, TRUE, 0);
	
	GtkWidget *vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_1), hbox_1, FALSE, FALSE, 0);
	
    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(scrolled_window, 140, 140);
	gtk_container_add(GTK_CONTAINER(scrolled_window), 
					  GTK_WIDGET(int_tree_view));
    gtk_box_pack_start(GTK_BOX(vbox_1), scrolled_window, FALSE, TRUE, 0);
    
	GtkWidget *Frame_1 = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_1), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame_1), vbox_1);
	
	expander = gtk_expander_new_with_mnemonic(duplicate_underscore(array_name_c));
	gtk_container_add(GTK_CONTAINER(expander), Frame_1);
	
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(int_tree_view));
 	GtkWidget *label = gtk_label_new("Values");   
    g_object_set_data(G_OBJECT(selection), "label", label);
    
    model = gtk_tree_view_get_model(GTK_TREE_VIEW(int_tree_view));
    child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    list = g_object_get_data(G_OBJECT(child_model), "selection_list");
    list = g_list_append(list, selection);
    g_object_set_data(G_OBJECT(child_model), "selection_list", list);
	
	return expander;
};
