/*
//  tree_view_block_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/21.
*/

#include "tree_view_block_GTK.h"

/* Append new data at the end of list */
int append_void_item_to_tree(int index, const char *label, GtkTreeModel *child_model){
    GtkTreeIter iter;
    
    gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_INDEX, index,
                       COLUMN_FIELD_NAME,  label,
                       -1);
    return index + 1;
}

int append_void_list_from_ctl(int index, struct void_ctl_list *head, 
							  GtkTreeView *v_tree_view)
{
    GtkTreeModel *model = gtk_tree_view_get_model (v_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    head = head->_next;
    while (head != NULL){
        index = append_void_item_to_tree(index, head->list_label, child_model);
        head = head->_next;
    };
    return index;
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

int add_void_list_by_bottun_GTK(int index, void *void_in, GtkTreeView *tree_view_to_add, 
								struct void_clist *v_clist)
{
    GtkTreeModel *model_to_add = gtk_tree_view_get_model(tree_view_to_add);
    GtkTreeModel *child_model_to_add = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_add));
        
	append_void_clist(void_in, v_clist);
	index = count_void_clist(v_clist);
    index = append_void_item_to_tree(index, void_clist_label_at_index(index,v_clist), child_model_to_add);
	
    return index;
}

int add_void_list_items_GTK(GtkTreeView *tree_view_to_add, 
							void *(*append_ctl_block_F)(int idx, char *block_name, void *f_parent), 
							void *(*init_block_item)(int idx, void *f_parent, void *void_in_gtk),
							void *(*dealloc_block_item)(void *f_block),
                            void *void_in_gtk, struct void_clist *v_clist)
{
    GtkTreeModel *model_to_add;
    GtkTreeModel *child_model_to_add;
    
    /* Get path of selected raw */
    /* The path is for tree_model_sort */
    model_to_add = gtk_tree_view_get_model(tree_view_to_add);
    child_model_to_add = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_add));
        
    
    /* Temporary block the changed signal of GtkTreeSelection */
    block_changed_signal(G_OBJECT(child_model_to_add));
    
	/* Return reference into path and delete reference */
	void *void_in = NULL;
	int num = count_void_clist(v_clist);
	append_ctl_block_F(num, v_clist->clist_name, v_clist->f_parent);
	
	int idx;
	for(idx=0;idx<count_void_clist(v_clist);idx++){
        dealloc_block_item(void_clist_at_index(idx, v_clist));
	};
	append_void_clist(void_in, v_clist);
	
	for(idx=0;idx<count_void_clist(v_clist);idx++){
		void *void_in = init_block_item(idx, v_clist->f_parent, void_in_gtk);
		replace_void_clist_at_index(idx, void_in, v_clist);
	};
	
	gtk_list_store_clear(GTK_LIST_STORE(child_model_to_add));
	int index = append_void_list_from_ctl(num, &v_clist->c_item_head, tree_view_to_add);
    /* Release the block of changed signal */
	unblock_changed_signal(G_OBJECT(child_model_to_add));
	return index;
}

void delete_void_list_items_GTK(GtkTreeView *tree_view_to_del,
								void *(*delete_ctl_block_F)(int idx, void *f_parent), 
                                void *(*init_block_item)(int idx, void *f_parent, void *void_in_gtk),
                                void *(*dealloc_block_item)(void *f_block),
                                void *void_in_gtk, struct void_clist *v_clist)
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
	int i, idx;
    for (cur = g_list_first(reference_list); cur != NULL; cur = g_list_next(cur)) {
        GtkTreePath *tree_path;
        GtkTreeIter iter;
        tree_path = gtk_tree_row_reference_get_path((GtkTreeRowReference *)cur->data);
        gtk_tree_model_get_iter(child_model_to_del, &iter, tree_path);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_NAME, &old_strng, -1);
        
        /* Delete */
        gtk_list_store_remove(GTK_LIST_STORE(child_model_to_del), &iter);

        gtk_tree_path_free(tree_path);
        gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
        
		/* Update control data */
		idx = find_void_clist_index_by_c_tbl(old_strng, v_clist);
		delete_ctl_block_F(idx, v_clist->f_parent);
		
		for(i=0;i<count_void_clist(v_clist);i++){
            dealloc_block_item(void_clist_at_index(i, v_clist));
		};
		del_void_clist_by_index(idx, v_clist);
		
		for(i=0;i<count_void_clist(v_clist);i++){
			void *void_in = init_block_item(i, v_clist->f_parent, void_in_gtk);
			replace_void_clist_at_index(i, void_in, v_clist);
		};
	}
    g_list_free(reference_list);
    /* Release the block of changed signal */
	unblock_changed_signal(G_OBJECT(child_model_to_del));
}

void create_block_tree_view(GtkTreeView *v_tree_view, GtkCellRenderer *renderer_text)
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
    gtk_tree_view_set_model(GTK_TREE_VIEW(v_tree_view), model);
    
    /* First raw */
	column_1st = gtk_tree_view_column_new();
    gtk_tree_view_append_column(v_tree_view, column_1st);
    gtk_tree_view_column_set_title(column_1st, "Field name");
    gtk_tree_view_column_set_resizable(column_1st, TRUE);
    gtk_tree_view_column_set_clickable(column_1st, TRUE);
	g_object_set_data(G_OBJECT(column_1st), "column_id", GINT_TO_POINTER(COLUMN_FIELD_NAME));
	/*
    g_signal_connect(G_OBJECT(column_1st), "clicked", 
					 G_CALLBACK(column_clicked), (gpointer) v_tree_view);
    */
    gtk_tree_view_column_pack_start(column_1st, renderer_text, TRUE);
    gtk_tree_view_column_set_attributes(column_1st, renderer_text, "text", COLUMN_FIELD_NAME, NULL);
	g_object_set(renderer_text, "width", (gint)240, NULL);
	
    /* 選択モード */
    selection = gtk_tree_view_get_selection(v_tree_view);
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
    column = gtk_tree_view_get_column(v_tree_view, COLUMN_FIELD_INDEX);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_INDEX, GTK_SORT_ASCENDING);
}


GtkWidget * add_block_list_box_w_addbottun(struct void_clist *v_clist_gtk, GtkWidget *v_tree_view, 
										   GtkWidget *button_add, GtkWidget *button_delete,
										   GtkWidget *vbox_out){
	GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	
	GtkCellRenderer *renderer_text =  gtk_cell_renderer_text_new();
	create_block_tree_view(GTK_TREE_VIEW(v_tree_view), renderer_text);
	v_clist_gtk->index_bc = append_void_list_from_ctl(v_clist_gtk->index_bc,
													  &v_clist_gtk->c_item_head,
													  GTK_TREE_VIEW(v_tree_view));

	GtkWidget *vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	
    /* Pack bottuns */
    GtkWidget *hbox_1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_pack_start(GTK_BOX(hbox_1), button_add,    FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox_1), button_delete, FALSE, FALSE, 0);
	
    gtk_box_pack_start(GTK_BOX(vbox_1), hbox_1, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_1), v_tree_view, TRUE, TRUE, 0);
	
	GtkWidget *Frame_1 = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_1), GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(Frame_1), vbox_1);
	gtk_box_pack_start(GTK_BOX(vbox), Frame_1, FALSE, TRUE, 0);
	return vbox;
};
