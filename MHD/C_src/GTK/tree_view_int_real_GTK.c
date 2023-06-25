/*
//  tree_view_int_real_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "tree_view_int_real_GTK.h"

void init_ir_clist_views(struct int_real_clist *ir_clist_gtk, struct ir_clist_view *ir_vws){
	ir_vws->ir_clist_gtk = ir_clist_gtk;
	return;
};

/* Append new data at the end of list */
int append_ir_item_to_tree(int index, double r1_data, double r2_data, 
			GtkTreeModel *child_model){
    GtkTreeIter iter;
    
    gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_INDEX, r1_data,
                       COLUMN_FIELD_NAME,  r2_data,
                       -1);
    return index + 1;
}

int append_ir_list_from_ctl(int index, struct int_real_ctl_list *head, 
			GtkTreeView *ir_tree_view){
    GtkTreeModel *model = gtk_tree_view_get_model (ir_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    head = head->_next;
    while (head != NULL){
		index = append_ir_item_to_tree(index, head->ir_item->i_data, 
					head->ir_item->r_data, child_model);
        head = head->_next;
    };
    return index;
}


void ir_tree_value1_edited(gchar *path_str, gchar *new_text,
			 GtkTreeView *ir_tree_view, struct int_real_clist *ir_clist_gtk){
	GtkTreeModel *model = gtk_tree_view_get_model (ir_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;
    
    double old_value1, old_value2, new_value;
    
    if(sscanf(new_text, "%lf", &new_value) < 1) return;
    gtk_tree_model_get_iter(child_model, &iter, child_path);  
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &old_value1, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &old_value2, -1);
    
    printf("Change %lf to %lf\n", old_value1, new_value);
    
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_INDEX, new_value, -1);
    gtk_tree_path_free(child_path);
    gtk_tree_path_free(path);
    
	update_int_real_clist_by_c_tbl(old_value1, old_value2, 
				new_value, old_value2, ir_clist_gtk);
};

void ir_tree_value2_edited(gchar *path_str, gchar *new_text,
			 GtkTreeView *ir_tree_view, struct int_real_clist *ir_clist_gtk){
	GtkTreeModel *model = gtk_tree_view_get_model (ir_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;
    
    double old_value1, old_value2, new_value;
    
    if(sscanf(new_text, "%lf", &new_value) < 1) return;
    gtk_tree_model_get_iter(child_model, &iter, child_path);  
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &old_value1, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &old_value2, -1);
    
    printf("Change %lf to %lf\n", old_value2, new_value);
    
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_NAME, new_value, -1);
    gtk_tree_path_free(child_path);
    gtk_tree_path_free(path);
    
    update_int_real_clist_by_c_tbl(old_value1, old_value2, old_value1, new_value, ir_clist_gtk);
}

static void column_clicked(GtkTreeViewColumn *column, gpointer user_data){
    GtkTreeView *ir_tree_view = GTK_TREE_VIEW(user_data);
    GtkTreeModel *model;
    gint column_id;
    gint cur_id;
    GtkSortType order;
    GtkTreeViewColumn *cur_column;
        
    if (gtk_widget_is_focus(GTK_WIDGET(ir_tree_view)) == FALSE) {
        gtk_widget_grab_focus(GTK_WIDGET(ir_tree_view));
    }
    
    column_id = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(column), "column_id"));
    model = gtk_tree_view_get_model(ir_tree_view);
    
    /* 現在のソート列と同じときは昇順／降順を反転する、違うときはクリックした列で昇順ソートする */
    if (gtk_tree_sortable_get_sort_column_id(GTK_TREE_SORTABLE(model), &cur_id, &order) == TRUE) {
        if (cur_id == column_id) {
            order = (order == GTK_SORT_ASCENDING) ? GTK_SORT_DESCENDING : GTK_SORT_ASCENDING;
        } else {
            order = GTK_SORT_ASCENDING;
        }
        cur_column = gtk_tree_view_get_column(ir_tree_view, cur_id);
        gtk_tree_view_column_set_sort_indicator(cur_column, FALSE);
    } else {
        order = GTK_SORT_ASCENDING;
    }
    gtk_tree_view_column_set_sort_order(column, order);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), column_id, order);
}

int add_ir_list_items(GtkTreeView *ir_tree_view, struct int_real_clist *ir_clist_gtk){
	GtkTreeModel *model_to_add;
    GtkTreeModel *child_model_to_add;
    GtkTreeSelection *selection;
    GList *list;
    GList *reference_list;
    GList *cur;
    int   ivalue;
    double rvalue;
	int index = 0;
    
    /* Get path of selected raw */
    /* The path is for tree_model_sort */
    model_to_add = gtk_tree_view_get_model(ir_tree_view);
    child_model_to_add = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_add));
        
    selection = gtk_tree_view_get_selection(ir_tree_view);
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
	
	GtkTreePath *tree_path;
	GtkTreeIter iter;
	cur = g_list_first(reference_list);
	tree_path = gtk_tree_row_reference_get_path((GtkTreeRowReference *)cur->data);
	gtk_tree_model_get_iter(child_model_to_add, &iter, tree_path);
	gtk_tree_model_get(child_model_to_add, &iter, COLUMN_FIELD_INDEX, &ivalue, -1);
	gtk_tree_model_get(child_model_to_add, &iter, COLUMN_FIELD_NAME, &rvalue, -1);
    for (cur = g_list_first(reference_list); cur != NULL; cur = g_list_next(cur)) {
        
        /* Add */
		add_int_real_clist_before_c_tbl(ivalue, rvalue, ir_clist_gtk);
		
		gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
		
	}
	
	gtk_tree_path_free(tree_path);
    g_list_free(reference_list);
	
	gtk_list_store_clear(GTK_LIST_STORE(child_model_to_add));
	index = append_ir_list_from_ctl(index, &ir_clist_gtk->ir_item_head, ir_tree_view);
	return index;
}

void delete_ir_list_items(GtkTreeView *ir_tree_view, struct int_real_clist *ir_clist_gtk){
    GtkTreeModel *model_to_del;
    GtkTreeModel *child_model_to_del;
    GtkTreeSelection *selection;
    GList *list;
    GList *reference_list;
    GList *cur;
	int ivalue;
    double rvalue;
    
    /* Get path of selected raw */
    /* The path is for tree_model_sort */
    model_to_del = gtk_tree_view_get_model(ir_tree_view);
    child_model_to_del = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_del));
    
    selection = gtk_tree_view_get_selection(ir_tree_view);
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
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_INDEX, &ivalue, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_NAME, &rvalue, -1);
        
        /* Delete */
        gtk_list_store_remove(GTK_LIST_STORE(child_model_to_del), &iter);
        
        gtk_tree_path_free(tree_path);
        gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
        
        /* Update control data */
        del_int_real_clist_by_c_tbl(ivalue, rvalue, ir_clist_gtk);
    }
    g_list_free(reference_list);
}

void create_ir_tree_view(GtkTreeView *ir_tree_view, struct int_real_clist *ir_clist_gtk, 
			GtkCellRenderer *renderer_spin1, GtkCellRenderer *renderer_spin2){
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
    gtk_tree_view_set_model(GTK_TREE_VIEW(ir_tree_view), model);
    
    /* First raw */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(ir_tree_view, column);
    gtk_tree_view_column_set_title(column, ir_clist_gtk->i1_name);
	/*
    adjust = gtk_adjustment_new(2.5, -1.0e30, 1.0e30, 0.1,
                                100, 21474836);
    g_object_set(G_OBJECT(renderer_spin1), 
				"adjustment", adjust,
				"climb-rate", 0.5,
				"digits", 3, NULL);
	*/
    g_object_set(G_OBJECT(renderer_spin1), 
                 "editable", TRUE, 
                 "width", (gint)70, NULL);
    
    gtk_tree_view_column_pack_start(column, renderer_spin1, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer_spin1, "text", COLUMN_FIELD_INDEX, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_INDEX));
    g_signal_connect(G_OBJECT(column), "clicked", 
                     G_CALLBACK(column_clicked), (gpointer) ir_tree_view);
    
    /* Second row */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(ir_tree_view, column);
    gtk_tree_view_column_set_title(column, ir_clist_gtk->r1_name);
    g_object_set(G_OBJECT(renderer_spin2), 
                 "editable", TRUE, 
                 "width", (gint)70, NULL);
    
    gtk_tree_view_column_pack_start(column, renderer_spin2, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer_spin2, "text", COLUMN_FIELD_NAME, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_NAME));
    g_signal_connect(G_OBJECT(column), "clicked", 
                     G_CALLBACK(column_clicked), (gpointer) ir_tree_view);
    
    /* 選択モード */
    selection = gtk_tree_view_get_selection(ir_tree_view);
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
    column = gtk_tree_view_get_column(ir_tree_view, COLUMN_FIELD_INDEX);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_INDEX, GTK_SORT_ASCENDING);
}

void add_ir_list_box(GtkTreeView *ir_tree_view, struct int_real_clist *ir_clist_gtk, 
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
    gtk_widget_set_size_request(scrolled_window, 140, 140);
    gtk_container_add(GTK_CONTAINER(scrolled_window), GTK_WIDGET(ir_tree_view));
    gtk_box_pack_start(GTK_BOX(vbox_1), scrolled_window, FALSE, TRUE, 0);
    
	Frame_1 = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_1), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame_1), vbox_1);
	
	expander = gtk_expander_new_with_mnemonic(ir_clist_gtk->clist_name);
	gtk_container_add(GTK_CONTAINER(expander), Frame_1);
	gtk_box_pack_start(GTK_BOX(vbox), expander, FALSE, TRUE, 0);
	
	
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(ir_tree_view));
    g_object_set_data(G_OBJECT(selection), "label", label);
    
    model = gtk_tree_view_get_model(GTK_TREE_VIEW(ir_tree_view));
    child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    list = g_object_get_data(G_OBJECT(child_model), "selection_list");
    list = g_list_append(list, selection);
    g_object_set_data(G_OBJECT(child_model), "selection_list", list);
	
	return;
};



void ir_tree_value1_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    struct ir_clist_view *ir_vws = (struct ir_clist_view *) user_data;
    ir_tree_value1_edited(path_str, new_text, GTK_TREE_VIEW(ir_vws->ir_tree_view), ir_vws->ir_clist_gtk);
    write_int_real_clist(stdout, 0, "ivalue changed", ir_vws->ir_clist_gtk);
};

void ir_tree_value2_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data)
{
    struct ir_clist_view *ir_vws = (struct ir_clist_view *) user_data;
    ir_tree_value2_edited(path_str, new_text, GTK_TREE_VIEW(ir_vws->ir_tree_view), ir_vws->ir_clist_gtk);
    write_int_real_clist(stdout, 0, "real value changed", ir_vws->ir_clist_gtk);
};

void add_ir_list_items_cb(GtkButton *button, gpointer user_data){
    struct ir_clist_view *ir_vws = (struct ir_clist_view *) user_data;
	ir_vws->index_bc = add_ir_list_items(GTK_TREE_VIEW(ir_vws->ir_tree_view), ir_vws->ir_clist_gtk);
    write_int_real_clist(stdout, 0, "columns added", ir_vws->ir_clist_gtk);
};

void delete_ir_list_items_cb(GtkButton *button, gpointer user_data){
    struct ir_clist_view *ir_vws = (struct ir_clist_view *) user_data;
	delete_ir_list_items(GTK_TREE_VIEW(ir_vws->ir_tree_view), ir_vws->ir_clist_gtk);
    write_int_real_clist(stdout, 0, "columns deleted", ir_vws->ir_clist_gtk);
};


void init_ir_tree_view(struct ir_clist_view *ir_vws){
	GtkCellRenderer *renderer_spin1;
	GtkCellRenderer *renderer_spin2;
	
	ir_vws->ir_tree_view = gtk_tree_view_new();
	renderer_spin1 = gtk_cell_renderer_text_new();
	renderer_spin2 = gtk_cell_renderer_text_new();
	g_signal_connect(G_OBJECT(renderer_spin1), "edited", 
					 G_CALLBACK(ir_tree_value1_edited_cb), (gpointer) ir_vws);
	g_signal_connect(G_OBJECT(renderer_spin2), "edited", 
					 G_CALLBACK(ir_tree_value2_edited_cb), (gpointer) ir_vws);
	
	create_ir_tree_view(GTK_TREE_VIEW(ir_vws->ir_tree_view), ir_vws->ir_clist_gtk, 
                           renderer_spin1, renderer_spin2);
	
	ir_vws->index_bc = append_ir_list_from_ctl(ir_vws->index_bc,
				&ir_vws->ir_clist_gtk->ir_item_head, GTK_TREE_VIEW(ir_vws->ir_tree_view));
};

void add_ir_list_box_w_addbottun(struct ir_clist_view *ir_vws, GtkWidget *vbox){
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	
	add_ir_list_box(GTK_TREE_VIEW(ir_vws->ir_tree_view), ir_vws->ir_clist_gtk,
				button_add, button_delete, vbox);
	
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(add_ir_list_items_cb), (gpointer) ir_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_ir_list_items_cb), (gpointer) ir_vws);
    
};

