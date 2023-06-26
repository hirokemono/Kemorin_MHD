/*
//  tree_view_int_real_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "tree_view_int_real_GTK.h"

static void copy_f_ctl_ir_array_by_r_list(struct int_real_clist *ir_clist,
										  struct f_ctl_ir_array *f_ir_array)
{
	int i;
	for(i=0;i<f_ir_array->f_num[0];i++){
		f_ir_array->f_ictls[i] = int_real_clist_at_index(i, ir_clist)->i_data;
		f_ir_array->f_rctls[i] = int_real_clist_at_index(i, ir_clist)->r_data;
	}
    return;
}

void update_f_ctl_ir_array_by_r_list(struct int_real_clist *ir_clist,
									   struct f_ctl_ir_array *f_ir_array)
{
/*	c_check_int_real_array(f_ir_array->f_self);*/
	copy_f_ctl_ir_array_by_r_list(ir_clist, f_ir_array);
/*	c_check_int_real_array(f_ir_array->f_self);*/
    return;
}
void reflesh_f_ctl_ir_array_by_r_list(struct int_real_clist *ir_clist,
									   struct f_ctl_ir_array *f_ir_array)
{
/*	c_check_int_real_array(f_ir_array->f_self);*/
	int num_array = count_int_real_clist(ir_clist);
	reflesh_f_ctl_ir_array(num_array, f_ir_array);
	copy_f_ctl_ir_array_by_r_list(ir_clist, f_ir_array);
/*	c_check_int_real_array(f_ir_array->f_self);*/
    return;
}


/* Append new data at the end of list */
static int append_ir_item_to_tree(const int index, const int i1_data, const double r2_data, 
			GtkTreeModel *child_model){
    GtkTreeIter iter;
    
		printf("append_ir_item_to_tree %d %le\n ", i1_data, r2_data);
    gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_INDEX, i1_data,
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
    
	double old_value2;
	int old_int, new_int;
    
    if(sscanf(new_text, "%d", &new_int) < 1) return;
    gtk_tree_model_get_iter(child_model, &iter, child_path);  
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &old_int, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &old_value2, -1);
    
    printf("Change %d to %d\n", old_int, new_int);
    
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_INDEX, new_int, -1);
    gtk_tree_path_free(child_path);
    gtk_tree_path_free(path);
    
	update_int_real_clist_by_c_tbl(old_int, old_value2, 
								   new_int, old_value2, ir_clist_gtk);
};

void ir_tree_value2_edited(gchar *path_str, gchar *new_text,
						   GtkTreeView *ir_tree_view, struct int_real_clist *ir_clist_gtk){
	GtkTreeModel *model = gtk_tree_view_get_model (ir_tree_view);
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;
    
    double old_value2, new_value;
	int old_int;
    
	sscanf(new_text, "%lf", &new_value);
    gtk_tree_model_get_iter(child_model, &iter, child_path);  
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &old_int, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &old_value2, -1);
    
    printf("Change %lf to %lf\n", old_value2, new_value);
    
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_NAME, new_value, -1);
    gtk_tree_path_free(child_path);
    gtk_tree_path_free(path);
	
	printf("%d %le %le\n", old_int, old_value2, new_value);
	
	update_int_real_clist_by_c_tbl(old_int, old_value2, 
								   old_int, new_value, ir_clist_gtk);
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
    child_model = gtk_list_store_new(2, G_TYPE_INT, G_TYPE_DOUBLE);
    g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);
    
    /* Construct model for sorting and set to tree view */
    model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    gtk_tree_view_set_model(GTK_TREE_VIEW(ir_tree_view), model);
    
    /* First raw */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(ir_tree_view, column);
    gtk_tree_view_column_set_title(column, ir_clist_gtk->i1_name);
    GtkAdjustment *adjust = gtk_adjustment_new(10, -32768, 32768, 1,
                                100, 21474836);
    g_object_set(G_OBJECT(renderer_spin1), 
                 "adjustment", adjust,
                 "climb-rate", 1,
                 "digits", 0, 
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


void cb_check_ir_array_toggle(GtkWidget *widget, gpointer user_data){
	GtkToggleButton *toggle = GTK_TOGGLE_BUTTON(widget);
	struct ir_clist_view *ir_vws = (struct ir_clist_view *) g_object_get_data(G_OBJECT(widget), "ir_clist_view");
	struct f_ctl_ir_array *f_ir_array = (struct f_ctl_ir_array *) g_object_get_data(G_OBJECT(widget), "f_ir_array");
	
	if(gtk_toggle_button_get_active(toggle) == TRUE){
		reflesh_f_ctl_ir_array(count_int_real_clist(ir_vws->ir_clist_gtk), f_ir_array);
		copy_f_ctl_ir_array_by_r_list(ir_vws->ir_clist_gtk, f_ir_array);
	}else{
		reflesh_f_ctl_ir_array(0, f_ir_array);
	};
	return;
}

GtkWidget *hbox_with_ir_array_checkbox(struct f_ctl_ir_array *f_ir_array, struct ir_clist_view *ir_vws){
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	GtkWidget *checkbox = gtk_check_button_new();
	g_object_set_data(G_OBJECT(checkbox), "ir_clist_view", (gpointer) ir_vws);
	g_object_set_data(G_OBJECT(checkbox), "f_ir_array", (gpointer) f_ir_array);
	
	if(f_ir_array->f_num[0] == 0){
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), FALSE);
	} else {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), TRUE);
	}
	
	g_signal_connect(G_OBJECT(checkbox), "toggled",
                     G_CALLBACK(cb_check_ir_array_toggle), NULL);
	
	gtk_box_pack_start(GTK_BOX(hbox), checkbox, TRUE, TRUE, 0);
	return hbox;
}



static void add_ir_list_box(struct f_ctl_ir_array *f_ir_array, struct ir_clist_view *ir_vws, 
			GtkWidget *button_add, GtkWidget *button_delete, GtkWidget *vbox){
	GtkWidget *expander, *Frame_1;
    
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
    
    /* Delete data bottun */
    
    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(scrolled_window, 140, 140);
    gtk_container_add(GTK_CONTAINER(scrolled_window), GTK_WIDGET(ir_vws->ir_tree_view));
    gtk_box_pack_start(GTK_BOX(vbox_1), scrolled_window, FALSE, TRUE, 0);
    
	Frame_1 = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_1), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame_1), vbox_1);
	
	char * ctmp = strngcopy_from_f(f_ir_array->f_block_name);
	expander = gtk_expander_new_with_mnemonic(duplicate_underscore(ctmp));
	gtk_container_add(GTK_CONTAINER(expander), Frame_1);
	
	GtkWidget * hbox1 = hbox_with_ir_array_checkbox(f_ir_array, ir_vws);
	GtkWidget *vbox0 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	gtk_box_pack_start(GTK_BOX(vbox0), hbox1, FALSE, TRUE, 0);
	
	GtkWidget *hbox0 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	gtk_box_pack_start(GTK_BOX(hbox0), vbox0, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox0), expander, FALSE, TRUE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox), hbox0, FALSE, TRUE, 0);
	
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(ir_vws->ir_tree_view));
 	GtkWidget *label = gtk_label_new("Values");   
    g_object_set_data(G_OBJECT(selection), "label", label);
    
    model = gtk_tree_view_get_model(GTK_TREE_VIEW(ir_vws->ir_tree_view));
    child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    list = g_object_get_data(G_OBJECT(child_model), "selection_list");
    list = g_list_append(list, selection);
    g_object_set_data(G_OBJECT(child_model), "selection_list", list);
	
	return;
};



static void ir_tree_value1_edited_cb(GtkCellRenderer *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    struct ir_clist_view *ir_vws = (struct ir_clist_view *) user_data;
	struct f_ctl_ir_array *f_ir_array = (struct f_ctl_ir_array *) g_object_get_data(G_OBJECT(cell), "f_ir_array");
    ir_tree_value1_edited(path_str, new_text, GTK_TREE_VIEW(ir_vws->ir_tree_view), ir_vws->ir_clist_gtk);
	update_f_ctl_ir_array_by_r_list(ir_vws->ir_clist_gtk, f_ir_array);
};

static void ir_tree_value2_edited_cb(GtkCellRenderer *cell, gchar *path_str,
			gchar *new_text, gpointer user_data)
{
    struct ir_clist_view *ir_vws = (struct ir_clist_view *) user_data;
	struct f_ctl_ir_array *f_ir_array = (struct f_ctl_ir_array *) g_object_get_data(G_OBJECT(cell), "f_ir_array");
    ir_tree_value2_edited(path_str, new_text, GTK_TREE_VIEW(ir_vws->ir_tree_view), ir_vws->ir_clist_gtk);
	update_f_ctl_ir_array_by_r_list(ir_vws->ir_clist_gtk, f_ir_array);
};

void add_ir_list_items_cb(GtkButton *button, gpointer user_data){
    struct ir_clist_view *ir_vws = (struct ir_clist_view *) user_data;
	struct f_ctl_ir_array *f_ir_array = (struct f_ctl_ir_array *) g_object_get_data(G_OBJECT(button), "f_ir_array");
	ir_vws->index_bc = add_ir_list_items(GTK_TREE_VIEW(ir_vws->ir_tree_view), ir_vws->ir_clist_gtk);
	reflesh_f_ctl_ir_array_by_r_list(ir_vws->ir_clist_gtk, f_ir_array);
};

void delete_ir_list_items_cb(GtkButton *button, gpointer user_data){
    struct ir_clist_view *ir_vws = (struct ir_clist_view *) user_data;
	struct f_ctl_ir_array *f_ir_array = (struct f_ctl_ir_array *) g_object_get_data(G_OBJECT(button), "f_ir_array");
	delete_ir_list_items(GTK_TREE_VIEW(ir_vws->ir_tree_view), ir_vws->ir_clist_gtk);
	reflesh_f_ctl_ir_array_by_r_list(ir_vws->ir_clist_gtk, f_ir_array);
};


static void init_ir_tree_view(struct f_ctl_ir_array *f_ir_array, struct ir_clist_view *ir_vws){
	GtkCellRenderer *renderer_spin1;
	GtkCellRenderer *renderer_spin2;
	
	ir_vws->ir_tree_view = gtk_tree_view_new();
	renderer_spin1 = gtk_cell_renderer_text_new();
	renderer_spin2 = gtk_cell_renderer_text_new();
	g_object_set_data(G_OBJECT(renderer_spin1), "f_ir_array", (gpointer) f_ir_array);
	g_object_set_data(G_OBJECT(renderer_spin2), "f_ir_array", (gpointer) f_ir_array);
	
	g_signal_connect(G_OBJECT(renderer_spin1), "edited", 
					 G_CALLBACK(ir_tree_value1_edited_cb), (gpointer) ir_vws);
	g_signal_connect(G_OBJECT(renderer_spin2), "edited", 
					 G_CALLBACK(ir_tree_value2_edited_cb), (gpointer) ir_vws);
	
	create_ir_tree_view(GTK_TREE_VIEW(ir_vws->ir_tree_view), ir_vws->ir_clist_gtk, 
                           renderer_spin1, renderer_spin2);
	
	ir_vws->index_bc = append_ir_list_from_ctl(ir_vws->index_bc,
				&ir_vws->ir_clist_gtk->ir_item_head, GTK_TREE_VIEW(ir_vws->ir_tree_view));
};

GtkWidget *  add_ir_list_box_w_addbottun(struct f_ctl_ir_array *f_ir_array, 
										 struct ir_clist_view *ir_vws){
	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	init_ir_tree_view(f_ir_array, ir_vws);
	
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	g_object_set_data(G_OBJECT(button_add), "f_ir_array", (gpointer) f_ir_array);
	g_object_set_data(G_OBJECT(button_delete), "f_ir_array", (gpointer) f_ir_array);
	
	add_ir_list_box(f_ir_array, ir_vws, button_add, button_delete, vbox);
	
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(add_ir_list_items_cb), (gpointer) ir_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_ir_list_items_cb), (gpointer) ir_vws);
	return vbox;
};

