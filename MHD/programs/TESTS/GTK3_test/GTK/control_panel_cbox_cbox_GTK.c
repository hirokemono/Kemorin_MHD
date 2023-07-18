/*
//  control_panel_cbox_cbox_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_cbox_cbox_GTK.h"

extern int lengthchara_f(void);

extern void * c_chara_item_clength(void *f_ctl, int *length);

static void load_clist_to_chara2_array(struct chara2_clist *c2_clst){
    int i;
    for(i=0;i<count_chara2_clist(c2_clst);i++){
        struct chara2_ctl_item *c2_tmp = chara2_clist_at_index(i, c2_clst);
        c_store_chara2_array(c2_clst->f_self, i, c2_tmp->c1_tbl, c2_tmp->c2_tbl);
    }
    return;
}

void c2_tree_text1_edited(gchar *path_str, gchar *new_text,
			GtkTreeView *c_tree_view, struct chara2_clist *c2_clist)
{
    GtkTreeModel *model = gtk_tree_view_get_model (c_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;

    gchar *old_text, *text2;

    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &old_text, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_MATH, &text2, -1);
    
    printf("Change text1 %s to %s\n", old_text, new_text);

    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_NAME, new_text, -1);
    gtk_tree_path_free(child_path);  
    gtk_tree_path_free(path);  
    
    update_chara2_clist_by_c_tbl(old_text, text2, new_text, text2, c2_clist);
}

void c2_tree_text2_edited(gchar *path_str, gchar *new_text,
			GtkTreeView *c_tree_view, struct chara2_clist *c2_clist)
{
    GtkTreeModel *model = gtk_tree_view_get_model (c_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;

    gchar *old_text, *text1;

    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &text1, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_MATH, &old_text, -1);
    
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_MATH, new_text, -1);
    gtk_tree_path_free(child_path);  
    gtk_tree_path_free(path);  
    
    update_chara2_clist_by_c_tbl(text1, old_text, text1, new_text, c2_clist);
}

static void c_combobox1_edited_cb(GtkCellRendererText *cell, gchar *path_str,
            gchar *new_text, gpointer user_data)
{
    struct chara2_cbox_table_view *chara2_tbl_vws = (struct chara2_cbox_table_view *) user_data;
	struct chara2_clist *ctl_clist = (struct chara2_clist *) g_object_get_data(G_OBJECT(cell), "chara2_clist");
    
    c2_tree_text1_edited(path_str, new_text, GTK_TREE_VIEW(chara2_tbl_vws->clist_tree_view), ctl_clist);
    load_clist_to_chara2_array(ctl_clist);
}

static void c_combobox2_edited_cb(GtkCellRendererText *cell, gchar *path_str,
            gchar *new_text, gpointer user_data)
{
    struct chara2_cbox_table_view *chara2_tbl_vws = (struct chara2_cbox_table_view *) user_data;
	struct chara2_clist *ctl_clist = (struct chara2_clist *) g_object_get_data(G_OBJECT(cell), "chara2_clist");
    
    c2_tree_text2_edited(path_str, new_text, GTK_TREE_VIEW(chara2_tbl_vws->clist_tree_view), ctl_clist);
    load_clist_to_chara2_array(ctl_clist);
}

int append_c2_item_to_tree_w_index(int index, const char *c1_tbl, const char *c2_tbl, 
                                   GtkTreeModel *child_model){
    GtkTreeIter iter;
    
    gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_INDEX, index,
                       COLUMN_FIELD_NAME,  c1_tbl,
                       COLUMN_FIELD_MATH,  c2_tbl,
                       -1);
    return index + 1;
}

int add_c2_list_by_bottun_GTK(int index, GtkTreeView *tree_view_to_add, 
                              struct chara2_clist *c2_clist)
{
    GtkTreeModel *model_to_add = gtk_tree_view_get_model(tree_view_to_add);
    GtkTreeModel *child_model_to_add = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_add));
        
    gchar row_string1[30] = "new_item1";
    gchar row_string2[30] = "new_item2";
    
    index = append_c2_item_to_tree_w_index(index, row_string1, row_string2, child_model_to_add);
	append_chara2_clist(row_string1, row_string2, c2_clist);
	
    return index;
}

static void cb_add_c2_item(GtkButton *button, gpointer user_data)
{
    struct chara2_cbox_table_view *chara2_tbl_vws = (struct chara2_cbox_table_view *) user_data;
	struct chara2_clist *ctl_clist = (struct chara2_clist *) g_object_get_data(G_OBJECT(button), "chara2_clist");
    
    chara2_tbl_vws->index_bc = add_c2_list_by_bottun_GTK(chara2_tbl_vws->index_bc,
                                                       GTK_TREE_VIEW(chara2_tbl_vws->clist_tree_view),
                                                       ctl_clist);
    reflesh_f_ctl_c2_array(count_chara2_clist(ctl_clist), ctl_clist);
    load_clist_to_chara2_array(ctl_clist);
    return;
}

void delete_c2_list_items_GTK(GtkTreeView *tree_view_to_del, struct chara2_clist *c2_clist)
{
    GtkTreeModel *model_to_del;
    GtkTreeModel *child_model_to_del;
    GtkTreeSelection *selection;
    GList *list;
    GList *reference_list;
    GList *cur;
    
    gchar *old_strng1, *old_strng2;
    
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
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_NAME, &old_strng1, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_MATH, &old_strng2, -1);
        
/*        printf("To be moved: %s\n", old_strng); */
        /* Delete */
        gtk_list_store_remove(GTK_LIST_STORE(child_model_to_del), &iter);

        gtk_tree_path_free(tree_path);
        gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
        
        /* Update control data */
        del_chara2_clist_by_c_tbl(old_strng1, old_strng2, c2_clist);
    }
    g_list_free(reference_list);
    
    /* Release the block of changed signal */
    unblock_changed_signal(G_OBJECT(child_model_to_del));
}

static void cb_delete_c_item_by_list(GtkButton *button, gpointer user_data)
{
    struct chara2_cbox_table_view *chara2_tbl_vws = (struct chara2_cbox_table_view *) user_data;
	struct chara2_clist *ctl_clist = (struct chara2_clist *) g_object_get_data(G_OBJECT(button), "chara2_clist");
    
    delete_c2_list_items_GTK(GTK_TREE_VIEW(chara2_tbl_vws->clist_tree_view), ctl_clist);
    reflesh_f_ctl_c2_array(count_chara2_clist(ctl_clist), ctl_clist);
    load_clist_to_chara2_array(ctl_clist);
}

void create_cbox_cbox_tree_view(GtkListStore *cbox_child_model1, GtkListStore *cbox_child_model2, GtkTreeView *c_tree_view,
                           GtkCellRenderer *renderer_cbox1, GtkCellRenderer *renderer_cbox2)
{
    /*    GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);*/
    
    GtkTreeModel *model;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    GtkListStore *child_model;

	/* Construct empty list storage */
    child_model = gtk_list_store_new(3, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING);
    g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);
    
    /* Construct model for sorting and set to tree view */
    model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    gtk_tree_view_set_model(c_tree_view, model);
    
    /* Second row */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(c_tree_view, column);
    gtk_tree_view_column_set_title(column, "Type1");
	g_object_set(G_OBJECT(renderer_cbox1), 
                 "text-column", FALSE, 
				"editable", TRUE, 
				"model", cbox_child_model1,
				"has-entry", TRUE, 
				"width", (gint)150, NULL);
    gtk_tree_view_column_pack_start(column, renderer_cbox1, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer_cbox1, "text", COLUMN_FIELD_NAME, NULL);
    gtk_tree_view_column_set_fixed_width(column, (gint) 160);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_NAME));
	/*
    g_signal_connect(G_OBJECT(column), "clicked", 
                     G_CALLBACK(column_clicked), (gpointer) c_tree_view);
	*/
    /* Third row */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(c_tree_view, column);
    gtk_tree_view_column_set_title(column, "Type2");
	g_object_set(G_OBJECT(renderer_cbox2), 
                 "text-column", FALSE, 
				"editable", TRUE, 
				"model", cbox_child_model2,
				"has-entry", TRUE, 
				"width", (gint)150, NULL);
    gtk_tree_view_column_pack_start(column, renderer_cbox2, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer_cbox2, "text", COLUMN_FIELD_MATH, NULL);
    gtk_tree_view_column_set_fixed_width(column, (gint) 160);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_MATH));
	/*
    g_signal_connect(G_OBJECT(column), "clicked", 
                     G_CALLBACK(column_clicked), (gpointer) c_tree_view);
	*/
	
    /* 選択モード */
    selection = gtk_tree_view_get_selection(c_tree_view);
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
    column = gtk_tree_view_get_column(c_tree_view, COLUMN_FIELD_NAME);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_NAME, GTK_SORT_ASCENDING);
}

int append_c2_list_from_ctl_w_index(int index, struct chara2_ctl_list *head,
                                    GtkTreeView *c_tree_view)
{
    GtkTreeModel *model = gtk_tree_view_get_model (c_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    head = head->_next;
    while (head != NULL){
        index = append_c2_item_to_tree_w_index(index, head->c2_item->c1_tbl, head->c2_item->c2_tbl, child_model);
        head = head->_next;
    };
    return index;
}


void init_c2_combobox_tree_view(struct chara2_clist *ctl_clist,
                                struct chara2_cbox_table_view *chara2_tbl_vws){
    GtkCellRenderer *renderer_cbox1 = gtk_cell_renderer_combo_new();
    GtkCellRenderer *renderer_cbox2 = gtk_cell_renderer_combo_new();
    GtkTreeModel *cbox_model1 = gtk_tree_view_get_model(GTK_TREE_VIEW(chara2_tbl_vws->item1_tree_view));
    GtkTreeModel *cbox_model2 = gtk_tree_view_get_model(GTK_TREE_VIEW(chara2_tbl_vws->item2_tree_view));
    create_cbox_cbox_tree_view(GTK_LIST_STORE(cbox_model1), GTK_LIST_STORE(cbox_model2),
                               GTK_TREE_VIEW(chara2_tbl_vws->clist_tree_view), renderer_cbox1, renderer_cbox2);
    
	g_object_set_data(G_OBJECT(renderer_cbox1), "chara2_clist", (gpointer) ctl_clist);
	g_object_set_data(G_OBJECT(renderer_cbox2), "chara2_clist", (gpointer) ctl_clist);
    g_signal_connect(G_OBJECT(renderer_cbox1), "edited",
                     G_CALLBACK(c_combobox1_edited_cb), (gpointer) chara2_tbl_vws);
    g_signal_connect(G_OBJECT(renderer_cbox2), "edited",
                     G_CALLBACK(c_combobox2_edited_cb), (gpointer) chara2_tbl_vws);

    chara2_tbl_vws->index_bc = append_c2_list_from_ctl_w_index(chara2_tbl_vws->index_bc, &ctl_clist->c2_item_head,
                                                              GTK_TREE_VIEW(chara2_tbl_vws->clist_tree_view));
}

void add_chara2_list_box_w_addbottun(GtkTreeView *c_tree_view,
                                     GtkWidget *button_add, GtkWidget *button_delete, 
                                     GtkWidget *vbox)
{
    GtkWidget *hbox;
    GtkWidget *scrolled_window;
    
    hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
    
    /* Pack bottuns */
    gtk_box_pack_start(GTK_BOX(hbox), button_add, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(hbox), button_delete, FALSE, FALSE, 0);
    
    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(scrolled_window, 240, 300);
    gtk_container_add(GTK_CONTAINER(scrolled_window), GTK_WIDGET(c_tree_view));
    gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, TRUE, TRUE, 0);
    
    add_sorting_signal_w_label(c_tree_view, hbox);
};

static void add_c2_list_selection_box(struct chara2_clist *ctl_clist, struct chara2_cbox_table_view *chara2_tbl_vws,
                                      GtkWidget *vbox)
{
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
    
    add_chara2_list_box_w_addbottun(GTK_TREE_VIEW(chara2_tbl_vws->clist_tree_view),
                                   button_add, button_delete, vbox);
    /* Add callbacks */
    
	g_object_set_data(G_OBJECT(button_add),    "chara2_clist", (gpointer) ctl_clist);
	g_object_set_data(G_OBJECT(button_delete), "chara2_clist", (gpointer) ctl_clist);
    
    g_signal_connect(G_OBJECT(button_add), "clicked",
                     G_CALLBACK(cb_add_c2_item), (gpointer) chara2_tbl_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked",
                     G_CALLBACK(cb_delete_c_item_by_list), (gpointer) chara2_tbl_vws);
};

struct chara2_cbox_table_view * init_chara2_cbox_table_view(struct chara_clist *item1_clist,
                                                           struct chara_clist *item2_clist){
    struct chara2_cbox_table_view *chara2_tbl_vws
            = (struct chara2_cbox_table_view *) malloc(sizeof(struct chara2_cbox_table_view));
    if(chara2_tbl_vws == NULL){
        printf("malloc error for chara2_cbox_table_view\n");
        exit(0);
    };
    
    chara2_tbl_vws->clist_tree_view = gtk_tree_view_new();
    chara2_tbl_vws->item1_tree_view = create_fixed_label_tree(item1_clist);
    chara2_tbl_vws->item2_tree_view = create_fixed_label_tree(item2_clist);
    return chara2_tbl_vws;
}


GtkWidget * c2_list_cbox_cbox_expander(struct chara2_clist *ctl_clist,
                                       struct chara_clist *item1_clist, struct chara_clist *item2_clist,
                                       struct chara2_cbox_table_view *chara2_tbl_vws,
                                       GtkWidget *window){
    chara2_tbl_vws = init_chara2_cbox_table_view(item1_clist, item2_clist);
    init_c2_combobox_tree_view(ctl_clist, chara2_tbl_vws);

    GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    add_c2_list_selection_box(ctl_clist, chara2_tbl_vws, vbox);
    GtkWidget *expand_bc = wrap_into_expanded_frame_gtk(duplicate_underscore(ctl_clist->clist_name),
                                                        window, vbox);
    return expand_bc;
}

