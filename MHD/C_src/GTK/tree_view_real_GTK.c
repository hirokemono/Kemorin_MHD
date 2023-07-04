/*
//  tree_view_real_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "tree_view_real_GTK.h"

/* Append new data at the end of list */
static int append_r_item_to_tree(int index, double r1_data, GtkTreeModel *child_model){
    GtkTreeIter iter;
    
    gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_INDEX, r1_data,
                       -1);
    return index + 1;
}

int append_r_list_from_ctl(int index, struct real_ctl_list *head, 
						   GtkTreeView *r_tree_view){
    GtkTreeModel *model = gtk_tree_view_get_model (r_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    head = head->_next;
    while (head != NULL){
		index = append_r_item_to_tree(index, head->r_item->r_data, child_model);
        head = head->_next;
    };
    return index;
}


void r_tree_value1_edited(gchar *path_str, gchar *new_text, GtkTreeView *r_tree_view, 
						  struct real_clist *r_clist){
	GtkTreeModel *model = gtk_tree_view_get_model (r_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;
    
    double old_value1, new_value;
    
	sscanf(new_text, "%lf", &new_value);
    gtk_tree_model_get_iter(child_model, &iter, child_path);  
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &old_value1, -1);
    
    printf("Change %lf to %lf\n", old_value1, new_value);
    
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_INDEX, new_value, -1);
    gtk_tree_path_free(child_path);
    gtk_tree_path_free(path);
    
	update_real_clist_by_c_tbl(old_value1, new_value, r_clist);
};

static void column_clicked(GtkTreeViewColumn *column, gpointer user_data){
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

int add_r_list_items(GtkTreeView *r_tree_view, struct real_clist *r_clist){
	GtkTreeModel *model_to_add;
    GtkTreeModel *child_model_to_add;
    GtkTreeSelection *selection;
    GList *list;
    GList *reference_list;
    GList *cur;
    double value = 0.0;
	int index = 0;
    
   	if(count_real_clist(r_clist) == 0){
		append_real_clist(value, r_clist);
		index = count_real_clist(r_clist);
        index = append_r_list_from_ctl(index, &r_clist->r_item_head, r_tree_view);
        return index;
    };
	
    
    /* Get path of selected raw */
    /* The path is for tree_model_sort */
    model_to_add = gtk_tree_view_get_model(r_tree_view);
    child_model_to_add = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_add));
        
    selection = gtk_tree_view_get_selection(r_tree_view);
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
    
    /* Add */
	
	GtkTreeIter iter;
	cur = g_list_first(reference_list);
	if(cur == NULL){
		append_real_clist(value, r_clist);
		index = count_real_clist(r_clist);
	} else {
		GtkTreePath *tree_path = gtk_tree_row_reference_get_path((GtkTreeRowReference *)cur->data);
		gtk_tree_model_get_iter(child_model_to_add, &iter, tree_path);
		gtk_tree_model_get(child_model_to_add, &iter, COLUMN_FIELD_INDEX, &value, -1);
		gtk_tree_path_free(tree_path);
		for (cur = g_list_first(reference_list); cur != NULL; cur = g_list_next(cur)) {
			/* Add */
			add_real_clist_before_c_tbl(value, r_clist);
			gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
		}
	};
	g_list_free(reference_list);
	
	gtk_list_store_clear(GTK_LIST_STORE(child_model_to_add));
	index = append_r_list_from_ctl(index, &r_clist->r_item_head, r_tree_view);
	return index;
}

void delete_r_list_items(GtkTreeView *r_tree_view, struct real_clist *r_clist){
    GtkTreeModel *model_to_del;
    GtkTreeModel *child_model_to_del;
    GtkTreeSelection *selection;
    GList *list;
    GList *reference_list;
    GList *cur;
    
    double value;
    
    /* Get path of selected raw */
    /* The path is for tree_model_sort */
    model_to_del = gtk_tree_view_get_model(r_tree_view);
    child_model_to_del = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_del));
    
    selection = gtk_tree_view_get_selection(r_tree_view);
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
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_INDEX, &value, -1);
        
        /* Delete */
        gtk_list_store_remove(GTK_LIST_STORE(child_model_to_del), &iter);
        
        gtk_tree_path_free(tree_path);
        gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
        
        /* Update control data */
        del_real_clist_by_c_tbl(value, r_clist);
    }
    g_list_free(reference_list);
}

void create_real_tree_view(GtkTreeView *r_tree_view, struct real_clist *r_clist,
						   GtkCellRenderer *renderer_spin1){
    /*    GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);*/
    
    GtkTreeModel *model;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    
    GtkListStore *child_model;
    /* GtkAdjustment *adjust; */
    
    /* Construct empty list storage */
    child_model = gtk_list_store_new(2, G_TYPE_DOUBLE, G_TYPE_DOUBLE);
    g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);
    
    /* Construct model for sorting and set to tree view */
    model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    gtk_tree_view_set_model(GTK_TREE_VIEW(r_tree_view), model);
    
    /* First raw */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(r_tree_view, column);
    gtk_tree_view_column_set_title(column, r_clist->r1_name);
    g_object_set(G_OBJECT(renderer_spin1), 
                 "editable", TRUE, "width", (gint)70, NULL);
    
    gtk_tree_view_column_pack_start(column, renderer_spin1, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer_spin1, "text", COLUMN_FIELD_INDEX, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
	g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_INDEX));
	/*
    g_signal_connect(G_OBJECT(column), "clicked", 
                     G_CALLBACK(column_clicked), (gpointer) r_tree_view);
    */
    /* 選択モード */
    selection = gtk_tree_view_get_selection(r_tree_view);
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
    column = gtk_tree_view_get_column(r_tree_view, COLUMN_FIELD_INDEX);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_INDEX, GTK_SORT_ASCENDING);
}

GtkWidget * real_list_box_expander(char *array_name_c, GtkWidget *real_array_tree_view,
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
					  GTK_WIDGET(real_array_tree_view));
    gtk_box_pack_start(GTK_BOX(vbox_1), scrolled_window, FALSE, TRUE, 0);
    
	GtkWidget *Frame_1 = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_1), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame_1), vbox_1);
	
	expander = gtk_expander_new_with_mnemonic(duplicate_underscore(array_name_c));
	gtk_container_add(GTK_CONTAINER(expander), Frame_1);
	
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(real_array_tree_view));
	GtkWidget *label = gtk_label_new("Real Value");
    g_object_set_data(G_OBJECT(selection), "label", label);
    
    model = gtk_tree_view_get_model(GTK_TREE_VIEW(real_array_tree_view));
    child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    list = g_object_get_data(G_OBJECT(child_model), "selection_list");
    list = g_list_append(list, selection);
    g_object_set_data(G_OBJECT(child_model), "selection_list", list);
	
	return expander;
};
