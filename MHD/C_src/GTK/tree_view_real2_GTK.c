/*
//  tree_view_real2_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "tree_view_real2_GTK.h"

void init_r2_clist_views(struct real2_clist *r2_clist, struct r2_clist_view *cmap_vws){
	cmap_vws->r2_clist_gtk = r2_clist;
	return;
};

/* Append new data at the end of list */
int append_r2_item_to_tree(int index, double r1_data, double r2_data, 
			GtkTreeModel *child_model){
    GtkTreeIter iter;
    
    gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_INDEX, r1_data,
                       COLUMN_FIELD_NAME,  r2_data,
                       -1);
    return index + 1;
}

int append_r2_list_from_ctl(int index, struct real2_ctl_list *head, 
			GtkTreeView *r2_tree_view){
    GtkTreeModel *model = gtk_tree_view_get_model (r2_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    head = head->_next;
    while (head != NULL){
		index = append_r2_item_to_tree(index, head->r2_item->r_data[0], 
					head->r2_item->r_data[1], child_model);
        head = head->_next;
    };
    return index;
}


void r2_tree_value1_edited(gchar *path_str, gchar *new_text,
			 GtkTreeView *r2_tree_view, struct real2_clist *r2_clist){
	GtkTreeModel *model = gtk_tree_view_get_model (r2_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;
    
    double old_value1, old_value2, new_value;
    
    sscanf(new_text, "%lf", &new_value);
    gtk_tree_model_get_iter(child_model, &iter, child_path);  
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &old_value1, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &old_value2, -1);
    
    printf("Change %lf to %lf\n", old_value1, new_value);
    
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_INDEX, new_value, -1);
    gtk_tree_path_free(child_path);
    gtk_tree_path_free(path);
    
	update_real2_clist_by_c_tbl(old_value1, old_value2, 
				new_value, old_value2, r2_clist);
};

void r2_tree_value2_edited(gchar *path_str, gchar *new_text,
			 GtkTreeView *r2_tree_view, struct real2_clist *r2_clist){
	GtkTreeModel *model = gtk_tree_view_get_model (r2_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;
    
    double old_value1, old_value2, new_value;
    
    sscanf(new_text, "%lf", &new_value);
    gtk_tree_model_get_iter(child_model, &iter, child_path);  
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &old_value1, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &old_value2, -1);
    
    printf("Change %lf to %lf\n", old_value2, new_value);
    
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_NAME, new_value, -1);
    gtk_tree_path_free(child_path);
    gtk_tree_path_free(path);
    
    update_real2_clist_by_c_tbl(old_value1, old_value2, old_value1, new_value, r2_clist);
}

static void column_clicked(GtkTreeViewColumn *column, gpointer user_data){
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

int add_r2_list_items(int index, GtkTreeView *r2_tree_view,
			struct real2_clist *r2_clist){
	GtkTreeModel *model_to_add;
    GtkTreeModel *child_model_to_add;
    GtkTreeSelection *selection;
    GList *list;
    GList *reference_list;
    GList *cur;
    gchar *field_name;
    double value1, value2;
    
    /* 選択されている行のパスを取得する */
    /* パスはツリーモデルソートのもの */
    model_to_add = gtk_tree_view_get_model(r2_tree_view);
    child_model_to_add = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_add));
        
    selection = gtk_tree_view_get_selection(r2_tree_view);
    list = gtk_tree_selection_get_selected_rows(selection, NULL);
    
	if(g_list_first(list) == NULL) return index;
    /* 最初にパスからリファレンスを作成する */
    /* データの削除を行なうと取得済みのパスが(大抵の場合)無効になる */
    reference_list = NULL;
    for (cur = g_list_first(list); cur != NULL; cur = g_list_next(cur)) {
        GtkTreePath *child_path;
        GtkTreeRowReference *child_reference;
        /* ツリーモデルソートのパスをツリーモデルのパスに変換する */
        child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model_to_add), 
                                                                    (GtkTreePath *)cur->data);
        
        child_reference = gtk_tree_row_reference_new(child_model_to_add, child_path);
        reference_list = g_list_append(reference_list, child_reference);
        
        gtk_tree_path_free(child_path);
        gtk_tree_path_free((GtkTreePath *)cur->data);
    }
    g_list_free(list);
    
    /* Add */
	
	GtkTreePath *tree_path;
	GtkTreeIter iter;
	cur = g_list_first(reference_list);
	tree_path = gtk_tree_row_reference_get_path((GtkTreeRowReference *)cur->data);
	gtk_tree_model_get_iter(child_model_to_add, &iter, tree_path);
	gtk_tree_model_get(child_model_to_add, &iter, COLUMN_FIELD_INDEX, &value1, -1);
	gtk_tree_model_get(child_model_to_add, &iter, COLUMN_FIELD_NAME, &value2, -1);
    for (cur = g_list_first(reference_list); cur != NULL; cur = g_list_next(cur)) {
        
        /* Add */
		add_real2_clist_before_c_tbl(value1, value2, r2_clist);
		
		gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
		
	}
	
	gtk_tree_path_free(tree_path);
    g_list_free(reference_list);
	
	gtk_list_store_clear(child_model_to_add);
	index = append_r2_list_from_ctl(index, &r2_clist->r2_item_head, r2_tree_view);
	return index;
}

void delete_r2_list_items(GtkTreeView *r2_tree_view, struct real2_clist *r2_clist){
    GtkTreeModel *model_to_del;
    GtkTreeModel *child_model_to_del;
    GtkTreeSelection *selection;
    GList *list;
    GList *reference_list;
    GList *cur;
    
    double value1, value2;
    
    /* 選択されている行のパスを取得する */
    /* パスはツリーモデルソートのもの */
    model_to_del = gtk_tree_view_get_model(r2_tree_view);
    child_model_to_del = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_del));
    
    selection = gtk_tree_view_get_selection(r2_tree_view);
	list = gtk_tree_selection_get_selected_rows(selection, NULL);
	if(g_list_first(list) == NULL) return;
    
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
    
	
	/* リファレンスをパスに戻して削除 */
    for (cur = g_list_first(reference_list); cur != NULL; cur = g_list_next(cur)) {
        GtkTreePath *tree_path;
        GtkTreeIter iter;
        tree_path = gtk_tree_row_reference_get_path((GtkTreeRowReference *)cur->data);
        gtk_tree_model_get_iter(child_model_to_del, &iter, tree_path);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_INDEX, &value1, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_NAME, &value2, -1);
        
        /* Delete */
        gtk_list_store_remove(GTK_LIST_STORE(child_model_to_del), &iter);
        
        gtk_tree_path_free(tree_path);
        gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
        
        /* Update control data */
        del_real2_clist_by_c_tbl(value1, value2, r2_clist);
    }
    g_list_free(reference_list);
}

void create_real2_tree_view(GtkTreeView *r2_tree_view, 
			GtkCellRenderer *renderer_spin1, GtkCellRenderer *renderer_spin2){
    /*    GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);*/
    
    GtkTreeModel *model;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    
    GtkListStore *child_model;
    GtkAdjustment *adjust;
    
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
                 "width", (gint)100, NULL);
    
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
//    adjust = gtk_adjustment_new(0.5, 0.0, 1.0, 0.01,
//                                100, 21474836);
	adjust = gtk_adjustment_new(0.5, -1.0e30, 1.0e30, 0.1,
                                100, 21474836);
	
    g_object_set(G_OBJECT(renderer_spin2), 
                 "adjustment", adjust,
                 "climb-rate", 0.1,
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

void add_real2_list_box(GtkTreeView *r2_tree_view, struct real2_clist *r2_clist, 
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
    gtk_widget_set_size_request(scrolled_window, 400, 300);
    gtk_container_add(GTK_CONTAINER(scrolled_window), r2_tree_view);
    gtk_box_pack_start(GTK_BOX(vbox_1), scrolled_window, TRUE, TRUE, 0);
    
	Frame_1 = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_1), GTK_SHADOW_IN);
	gtk_container_add(GTK_BOX(Frame_1), vbox_1);
	
	expander = gtk_expander_new_with_mnemonic(r2_clist->clist_name);
	gtk_container_add(GTK_CONTAINER(expander), Frame_1);
	gtk_box_pack_start(GTK_BOX(vbox), expander, TRUE, TRUE, 0);
	
	
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(r2_tree_view));
    g_object_set_data(G_OBJECT(selection), "label", label);
    
    model = gtk_tree_view_get_model(GTK_TREE_VIEW(r2_tree_view));
    child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    list = g_object_get_data(G_OBJECT(child_model), "selection_list");
    list = g_list_append(list, selection);
    g_object_set_data(G_OBJECT(child_model), "selection_list", list);
	
	return;
};



void r2_tree_value1_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    struct r2_clist_view *r2_vws = (struct boundary_condition_view *) user_data;
    r2_tree_value1_edited(path_str, new_text, r2_vws->tree_view, r2_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "value1 changed", r2_vws->r2_clist_gtk);
};

void r2_tree_value2_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data)
{
    struct r2_clist_view *r2_vws = (struct boundary_condition_view *) user_data;
    r2_tree_value2_edited(path_str, new_text, r2_vws->tree_view, r2_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "value2 changed", r2_vws->r2_clist_gtk);
};

void add_r2_list_items_cb(GtkButton *button, gpointer user_data){
    struct r2_clist_view *r2_vws = (struct r2_clist_view *) user_data;
	r2_vws->index_bc = add_r2_list_items(r2_vws->index_bc, 
				r2_vws->tree_view, r2_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "columns added", r2_vws->r2_clist_gtk);
};

void delete_r2_list_items_cb(GtkButton *button, gpointer user_data){
    struct r2_clist_view *r2_vws = (struct r2_clist_view *) user_data;
	delete_r2_list_items(r2_vws->tree_view, r2_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "columns deleted", r2_vws->r2_clist_gtk);
};


void init_real2_tree_view(struct r2_clist_view *r2_vws){
	GtkCellRenderer *renderer_spin1;
	GtkCellRenderer *renderer_spin2;
	
	r2_vws->tree_view = gtk_tree_view_new();
	renderer_spin1 = gtk_cell_renderer_spin_new();
	renderer_spin2 = gtk_cell_renderer_spin_new();
	
	create_real2_tree_view(r2_vws->tree_view, renderer_spin1, renderer_spin2);
	
    g_signal_connect(G_OBJECT(renderer_spin1), "edited", 
                     G_CALLBACK(r2_tree_value1_edited_cb), r2_vws);
    g_signal_connect(G_OBJECT(renderer_spin2), "edited", 
                     G_CALLBACK(r2_tree_value2_edited_cb), r2_vws);
	
	r2_vws->index_bc = append_r2_list_from_ctl(r2_vws->index_bc,
				&r2_vws->r2_clist_gtk->r2_item_head, r2_vws->tree_view);
};

void add_real2_list_box_w_addbottun(struct r2_clist_view *r2_vws, GtkWidget *vbox){
    GtkWidget *button_add = gtk_button_new_from_stock(GTK_STOCK_ADD);
    GtkWidget *button_delete = gtk_button_new_from_stock(GTK_STOCK_REMOVE);
	
	add_real2_list_box(r2_vws->tree_view, r2_vws->r2_clist_gtk,
				button_add, button_delete, vbox);
	
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(add_r2_list_items_cb), r2_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_r2_list_items_cb), r2_vws);
    
};

