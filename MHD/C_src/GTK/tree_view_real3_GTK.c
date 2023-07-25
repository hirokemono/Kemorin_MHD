/*
//  tree_view_real3_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/25.
*/

#include "tree_view_real3_GTK.h"

void init_r3_clist_views(struct real3_clist *r3_clist, struct r3_clist_view *cmap_vws){
    cmap_vws->r3_clist_gtk = r3_clist;
    return;
};

/* Append new data at the end of list */
int append_r3_item_to_tree(int index, double r1_data, double r2_data, double r3_data, 
                           GtkTreeModel *child_model){
    GtkTreeIter iter;
    
    gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_INDEX, r1_data,
                       COLUMN_FIELD_NAME,  r2_data,
                       COLUMN_FIELD_MATH,  r3_data,
                       -1);
    return index + 1;
}

int append_r3_list_from_ctl(int index, struct real3_clist *r3_clist, 
                            GtkTreeView *r3_tree_view){
    GtkTreeModel *model = gtk_tree_view_get_model (r3_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    struct real3_ctl_list *head = &r3_clist->r3_item_head;
    head = head->_next;
    while (head != NULL){
        index = append_r3_item_to_tree(index, head->r3_item->r_data[0], head->r3_item->r_data[1],
									   head->r3_item->r_data[2], child_model);
        head = head->_next;
    };
    return index;
}


void r3_tree_value1_edited(gchar *path_str, gchar *new_text,
                           GtkTreeView *r3_tree_view, struct real3_clist *r3_clist){
    GtkTreeModel *model = gtk_tree_view_get_model (r3_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;
    
    double old_value1, old_value2, old_value3, new_value;
    
	if(sscanf(new_text, "%lf", &new_value) < 1) return;
	gtk_tree_model_get_iter(child_model, &iter, child_path);  
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &old_value1, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &old_value2, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_MATH, &old_value3, -1);
    
    printf("Change %lf to %lf\n", old_value1, new_value);
    
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_INDEX, new_value, -1);
    gtk_tree_path_free(child_path);
    gtk_tree_path_free(path);
    
    update_real3_clist_by_c_tbl(old_value1, old_value2, old_value3, 
                                new_value, old_value2, old_value3, r3_clist);
};

void r3_tree_value2_edited(gchar *path_str, gchar *new_text,
                           GtkTreeView *r3_tree_view, struct real3_clist *r3_clist){
    GtkTreeModel *model = gtk_tree_view_get_model (r3_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;
    
    double old_value1, old_value2, old_value3, new_value;
    
	if(sscanf(new_text, "%lf", &new_value) < 1) return;
    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &old_value1, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &old_value2, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_MATH, &old_value3, -1);
    
    printf("Change %lf to %lf\n", old_value2, new_value);
    
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_NAME, new_value, -1);
    gtk_tree_path_free(child_path);
    gtk_tree_path_free(path);
    
    update_real3_clist_by_c_tbl(old_value1, old_value2, old_value3, 
                                old_value1, new_value, old_value3, r3_clist);
}

void r3_tree_value3_edited(gchar *path_str, gchar *new_text,
                           GtkTreeView *r3_tree_view, struct real3_clist *r3_clist){
    GtkTreeModel *model = gtk_tree_view_get_model (r3_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;
    
    double old_value1, old_value2, old_value3, new_value;
    
	if(sscanf(new_text, "%lf", &new_value) < 1) return;
    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &old_value1, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &old_value2, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_MATH, &old_value3, -1);
    
    printf("Change %lf to %lf\n", old_value3, new_value);
    
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_MATH, new_value, -1);
    gtk_tree_path_free(child_path);
    gtk_tree_path_free(path);
    
    update_real3_clist_by_c_tbl(old_value1, old_value2, old_value3, 
                                old_value1, old_value2, new_value, r3_clist);
}

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


GList * set_selected_r3_list_items(GtkTreeView *r3_tree_view, double org_value[3]){
	/* Get path of selected raw  */
    /* The path is for tree_model_sort */
    GtkTreeModel *model_to_change = gtk_tree_view_get_model(r3_tree_view);;
    GtkTreeModel *child_model_to_change = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_change));;
    GtkTreeSelection *selection = gtk_tree_view_get_selection(r3_tree_view);;
    GList *list = gtk_tree_selection_get_selected_rows(selection, NULL);;
    GList *reference_list = NULL;
    GList *cur;
    GtkTreePath *tree_path;
    GtkTreeIter iter;
	
    if(g_list_first(list) == NULL) return reference_list;
    /* Make reference from path */
    /* After deleting data, obtained path would not be valied */
    for (cur = g_list_first(list); cur != NULL; cur = g_list_next(cur)) {
        GtkTreePath *child_path;
        GtkTreeRowReference *child_reference;
        /* Convert tree model sort path into tree model path */
        child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model_to_change), 
                                                                    (GtkTreePath *)cur->data);
        
        child_reference = gtk_tree_row_reference_new(child_model_to_change, child_path);
        reference_list = g_list_append(reference_list, child_reference);
        
        gtk_tree_path_free(child_path);
        gtk_tree_path_free((GtkTreePath *)cur->data);
    }
    g_list_free(list);
    
    cur = g_list_first(reference_list);
    tree_path = gtk_tree_row_reference_get_path((GtkTreeRowReference *)cur->data);
    gtk_tree_model_get_iter(child_model_to_change, &iter, tree_path);
    gtk_tree_model_get(child_model_to_change, &iter, COLUMN_FIELD_INDEX, &org_value[0], -1);
    gtk_tree_model_get(child_model_to_change, &iter, COLUMN_FIELD_NAME, &org_value[1], -1);
    gtk_tree_model_get(child_model_to_change, &iter, COLUMN_FIELD_MATH, &org_value[2], -1);
	
	gtk_tree_path_free(tree_path);
	return reference_list;
}

int add_r3_list_items(GtkTreeView *r3_tree_view, struct real3_clist *r3_clist){
    GtkTreeModel *model_to_add;
    GtkTreeModel *child_model_to_add;
    GtkTreeSelection *selection;
    GList *list;
    GList *reference_list;
    GList *cur;
    double value1 = 0.0, value2 = 0.0, value3 = 0.0;
	int index = 0;
	
	
   	if(count_real3_clist(r3_clist) == 0){
		append_real3_clist(value1, value2, value3, r3_clist);
		index = count_real3_clist(r3_clist);
		index = append_r3_list_from_ctl(index, r3_clist, r3_tree_view);
        return index;
    };
	
    /* Get path of selected raw  */
    /* The path is for tree_model_sort */
    model_to_add = gtk_tree_view_get_model(r3_tree_view);
    child_model_to_add = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_add));
    
    selection = gtk_tree_view_get_selection(r3_tree_view);
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
		append_real3_clist(value1, value2, value3, r3_clist);
		index = count_real3_clist(r3_clist);
	} else {
		GtkTreePath *tree_path = gtk_tree_row_reference_get_path((GtkTreeRowReference *)cur->data);
		gtk_tree_model_get_iter(child_model_to_add, &iter, tree_path);
		gtk_tree_model_get(child_model_to_add, &iter, COLUMN_FIELD_INDEX, &value1, -1);
		gtk_tree_model_get(child_model_to_add, &iter, COLUMN_FIELD_NAME, &value2, -1);
		gtk_tree_model_get(child_model_to_add, &iter, COLUMN_FIELD_MATH, &value3, -1);
		gtk_tree_path_free(tree_path);
		
		for (cur = g_list_first(reference_list); cur != NULL; cur = g_list_next(cur)) {
        /* Add */
			add_real3_clist_before_c_tbl(value1, value2, value3, r3_clist);
			gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
		}
	};
    g_list_free(reference_list);
    
    gtk_list_store_clear(GTK_LIST_STORE(child_model_to_add));
    index = append_r3_list_from_ctl(index, r3_clist, r3_tree_view);
    return index;
}

void delete_r3_list_items(GtkTreeView *r3_tree_view, struct real3_clist *r3_clist){
    GtkTreeModel *model_to_del;
    GtkTreeModel *child_model_to_del;
    GtkTreeSelection *selection;
    GList *list;
    GList *reference_list;
    GList *cur;
    
    double value1, value2, value3;
    
    /* Get path of selected raw */
    /* The path is for tree_model_sort */
    model_to_del = gtk_tree_view_get_model(r3_tree_view);
    child_model_to_del = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_del));
    
    selection = gtk_tree_view_get_selection(r3_tree_view);
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
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_INDEX, &value1, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_NAME, &value2, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_MATH, &value3, -1);
        
        /* Delete */
        gtk_list_store_remove(GTK_LIST_STORE(child_model_to_del), &iter);
        
        gtk_tree_path_free(tree_path);
        gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
        
        /* Update control data */
        del_real3_clist_by_c_tbl(value1, value2, value3, r3_clist);
    }
    g_list_free(reference_list);
}

void create_real3_tree_view(GtkTreeView *r3_tree_view, struct real3_clist *r3_clist,
                            GtkCellRenderer *renderer_spin1, GtkCellRenderer *renderer_spin2,
                            GtkCellRenderer *renderer_spin3){
    /*    GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);*/
    
    GtkTreeModel *model;
    GtkTreeViewColumn *column_1st;
    GtkTreeViewColumn *column_2nd;
    GtkTreeViewColumn *column_3rd;
    GtkTreeSelection *selection;
    
    GtkListStore *child_model;
    /* GtkAdjustment *adjust; */
    
    /* Construct empty list storage */
    child_model = gtk_list_store_new(3, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE);
    g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);
    
    /* ソート用のモデルを作成してツリービューにセットする */
    model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    gtk_tree_view_set_model(r3_tree_view, model);
    
    /* First raw */
    column_1st = gtk_tree_view_column_new();
    gtk_tree_view_append_column(r3_tree_view, column_1st);
	gtk_tree_view_column_set_title(column_1st, r3_clist->r1_name);
	
    GtkAdjustment *adjust = gtk_adjustment_new(2.5, -1.0e30, 1.0e30, 0.1,
                                               100, 21474836);
/*
     g_object_set(G_OBJECT(renderer_spin1),
				"adjustment", adjust,
                "editable", TRUE,
                "climb-rate", 0.5,
                "digits", 6,
                 "width", (gint)80, NULL);
    */
    g_object_set(G_OBJECT(renderer_spin1),
                 "editable", TRUE,
                 "width", (gint)70, NULL);
    gtk_tree_view_column_pack_start(column_1st, renderer_spin1, TRUE);
	gtk_tree_view_column_set_attributes(column_1st, renderer_spin1, "text",
				COLUMN_FIELD_INDEX, NULL);
    gtk_tree_view_column_set_resizable(column_1st, TRUE);
    gtk_tree_view_column_set_clickable(column_1st, TRUE);
	g_object_set_data(G_OBJECT(column_1st), "column_id",
					  GINT_TO_POINTER(COLUMN_FIELD_INDEX));
	/*
    g_signal_connect(G_OBJECT(column_1st), "clicked",
				G_CALLBACK(column_clicked), (gpointer) r3_tree_view);
    */
    /* Second row */
    column_2nd = gtk_tree_view_column_new();
    gtk_tree_view_append_column(r3_tree_view, column_2nd);
    gtk_tree_view_column_set_title(column_2nd, r3_clist->r2_name);
/*

    g_object_set(G_OBJECT(renderer_spin2),
				"adjustment", adjust,
                "editable", TRUE,
                "climb-rate", 0.5,
                "digits", 6,
                 "width", (gint)80, NULL);
    */
    g_object_set(G_OBJECT(renderer_spin2),
                 "editable", TRUE,
                 "width", (gint)70, NULL);

    gtk_tree_view_column_pack_start(column_2nd, renderer_spin2, TRUE);
	gtk_tree_view_column_set_attributes(column_2nd, renderer_spin2, "text",
				COLUMN_FIELD_NAME, NULL);
    gtk_tree_view_column_set_resizable(column_2nd, TRUE);
    gtk_tree_view_column_set_clickable(column_2nd, TRUE);
	g_object_set_data(G_OBJECT(column_2nd), "column_id",
					  GINT_TO_POINTER(COLUMN_FIELD_NAME));
	/*
    g_signal_connect(G_OBJECT(column_2nd), "clicked",
                     G_CALLBACK(column_clicked), (gpointer) r3_tree_view);
    */
    /* Third row */
    column_3rd = gtk_tree_view_column_new();
    gtk_tree_view_append_column(r3_tree_view, column_3rd);
    gtk_tree_view_column_set_title(column_3rd, r3_clist->r3_name);
    /*
    g_object_set(G_OBJECT(renderer_spin3),
				"adjustment", adjust,
                "editable", TRUE,
                "climb-rate", 0.5,
                "digits", 6,
                 "width", (gint)80, NULL);
    */
    g_object_set(G_OBJECT(renderer_spin3),
                 "editable", TRUE,
                 "width", (gint)70, NULL);
    gtk_tree_view_column_pack_start(column_3rd, renderer_spin3, TRUE);
	gtk_tree_view_column_set_attributes(column_3rd, renderer_spin3, "text",
				COLUMN_FIELD_MATH, NULL);
    gtk_tree_view_column_set_resizable(column_3rd, TRUE);
    gtk_tree_view_column_set_clickable(column_3rd, TRUE);
	g_object_set_data(G_OBJECT(column_3rd), "column_id",
					  GINT_TO_POINTER(COLUMN_FIELD_MATH));
	/*
    g_signal_connect(G_OBJECT(column_3rd), "clicked",
				G_CALLBACK(column_clicked), (gpointer) r3_tree_view);
    */
    /* 選択モード */
    selection = gtk_tree_view_get_selection(r3_tree_view);
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
	/* sort */
    gtk_tree_view_column_set_sort_order(column_1st, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column_1st, TRUE);
	gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model),
					COLUMN_FIELD_INDEX, GTK_SORT_ASCENDING);
}

void add_real3_list_box(GtkTreeView *r3_tree_view, struct real3_clist *r3_clist,
                        GtkWidget *button_add, GtkWidget *button_delete, GtkWidget *vbox){
    GtkWidget *expander, *Frame_1;
    GtkWidget *hbox_1, *vbox_1;
    
    GtkWidget *label;
    GtkWidget *scrolled_window;
    
    GtkTreeSelection *selection;
    GtkTreeModel *model;
    GtkTreeModel *child_model;
    GList *list;
    
    char *c_label;
    
    c_label = (char *)calloc(KCHARA_C, sizeof(char));
    
    hbox_1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
    
    /* Pack bottuns */
    gtk_box_pack_start(GTK_BOX(hbox_1), button_add, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox_1), button_delete, FALSE, FALSE, 0);
    
    label = gtk_label_new("");
    gtk_box_pack_end(GTK_BOX(hbox_1), label, TRUE, TRUE, 0);
    
    vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_1), hbox_1, FALSE, FALSE, 0);
    
    /* Delete data bottun */
    
    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(scrolled_window, 240, 160);
    gtk_container_add(GTK_CONTAINER(scrolled_window), GTK_WIDGET(r3_tree_view));
    gtk_box_pack_start(GTK_BOX(vbox_1), scrolled_window, TRUE, TRUE, 0);
    
    Frame_1 = gtk_frame_new("");
    gtk_frame_set_shadow_type(GTK_FRAME(Frame_1), GTK_SHADOW_IN);
    gtk_container_add(GTK_CONTAINER(Frame_1), vbox_1);
    
    expander = gtk_expander_new_with_mnemonic(r3_clist->clist_name);
    gtk_container_add(GTK_CONTAINER(expander), Frame_1);
    gtk_box_pack_start(GTK_BOX(vbox), expander, TRUE, TRUE, 0);
    
    
    selection = gtk_tree_view_get_selection(r3_tree_view);
    g_object_set_data(G_OBJECT(selection), "label", label);
    
    model = gtk_tree_view_get_model(r3_tree_view);
    child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    list = g_object_get_data(G_OBJECT(child_model), "selection_list");
    list = g_list_append(list, selection);
    g_object_set_data(G_OBJECT(child_model), "selection_list", list);
    
    return;
};

