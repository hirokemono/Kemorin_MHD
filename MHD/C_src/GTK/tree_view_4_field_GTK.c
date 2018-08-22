/*
//  tree_view_4_field_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/14.
*/

#include "tree_view_4_field_GTK.h"

void init_field_views_GTK(struct field_ctl_c *fld_ctl_ref, struct field_views *fields_vws){
    fields_vws->fld_ctl_gtk = fld_ctl_ref;
    fields_vws->all_fld_tbl = (struct all_field_ctl_c **) malloc(NUM_FIELD * sizeof(struct all_field_ctl_c *));
    alloc_all_field_ctl_c(fields_vws->all_fld_tbl);
    load_field_w_qflag_from_ctl(fields_vws->fld_ctl_gtk, fields_vws->all_fld_tbl);
    return;
}

void dealloc_field_views_GTK(struct field_views *fields_vws){
    dealloc_all_field_ctl_c(fields_vws->all_fld_tbl);
    free(fields_vws->all_fld_tbl);
    free(fields_vws);
    return;
}


/* Append new data at the end of list */
void append_model_data(int index_field, struct all_field_ctl_c *all_fld_tbl, GtkTreeModel *child_model)
{
    GtkTreeIter iter;
    
    gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_INDEX, index_field,
                       COLUMN_FIELD_NAME, all_fld_tbl->field_name,
                       COLUMN_FIELD_MATH, all_fld_tbl->field_math,
                       COLUMN_VIZ_FLAG, (gboolean) all_fld_tbl->iflag_viz,
                       COLUMN_MONITOR_FLAG, (gboolean) all_fld_tbl->iflag_monitor,
                       COLUMN_NUM_COMP, all_fld_tbl->num_comp,
                       COLUMN_QUADRATURE, all_fld_tbl->iflag_quad,
                       -1);
}

static void column_clicked(GtkTreeViewColumn *column, gpointer user_data)
{
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

static void toggle_viz_switch(GtkTreeViewColumn *renderer, 
			gchar *path_str, gpointer user_data){
	struct field_views *fields_vws = (struct field_views *) user_data;
    GtkTreeModel *model = gtk_tree_view_get_model (fields_vws->used_tree_view);  
	GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;

    gchar *row_string;
    int index_field, index_for_toggle;

    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_VIZ_FLAG, &index_for_toggle, -1);
    
   
    printf("toggle_viz_switch %d, %s: %s\n", index_field, row_string,
            fields_vws->all_fld_tbl[index_field]->field_name);
    
    index_for_toggle = (index_for_toggle+ 1) % 2;
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_VIZ_FLAG, (gboolean) index_for_toggle, -1);
    gtk_tree_path_free(child_path);  
    gtk_tree_path_free(path);  
    
    fields_vws->all_fld_tbl[index_field]->iflag_viz = index_for_toggle;
	update_field_flag_wqflag_in_ctl(fields_vws->all_fld_tbl[index_field],
				fields_vws->fld_ctl_gtk);
}
static void toggle_monitor_switch(GtkTreeViewColumn *renderer, gchar *path_str, gpointer user_data){
	struct field_views *fields_vws = (struct field_views *) user_data;
    GtkTreeModel *model = gtk_tree_view_get_model (fields_vws->used_tree_view);  
	GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;

    gchar *row_string;
    int index_field, index_for_toggle;
    
    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &row_string, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_MONITOR_FLAG, &index_for_toggle, -1);
    
    printf("toggle_monitor_switch %d, %s: %s\n", index_field, row_string,
           fields_vws->all_fld_tbl[index_field]->field_name);
	
    index_for_toggle = (index_for_toggle+ 1) % 2;
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_MONITOR_FLAG, (gboolean) index_for_toggle, -1);
    gtk_tree_path_free(child_path);  
    gtk_tree_path_free(path);  
    
    fields_vws->all_fld_tbl[index_field]->iflag_monitor = index_for_toggle;
	update_field_flag_wqflag_in_ctl(fields_vws->all_fld_tbl[index_field],
				fields_vws->fld_ctl_gtk);
}



void create_field_tree_view(struct field_views *fields_vws)
{
    /*    GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);*/
    GtkTreeModel *model;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    
    GtkListStore *child_model;
    
    int i;
    
    /* Construct empty list storage */
    child_model = gtk_list_store_new(7, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING,
                                     G_TYPE_INT, G_TYPE_INT, G_TYPE_INT, G_TYPE_INT);
    g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);
    
    /* ソート用のモデルを作成してツリービューにセットする */
    model = gtk_tree_model_sort_new_with_model(child_model);
    gtk_tree_view_set_model(GTK_TREE_VIEW(fields_vws->used_tree_view), model);
    
    /* First raw */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(GTK_TREE_VIEW(fields_vws->used_tree_view), column);
    gtk_tree_view_column_set_title(column, "Index");
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(column, renderer, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_FIELD_INDEX, NULL);
    g_object_set(renderer, "width", (gint)60, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_INDEX));
    g_signal_connect(G_OBJECT(column), "clicked", G_CALLBACK(column_clicked), fields_vws->used_tree_view);
    
    /* Second row */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(GTK_TREE_VIEW(fields_vws->used_tree_view), column);
    gtk_tree_view_column_set_title(column, "Field name");
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(column, renderer, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_FIELD_NAME, NULL);
    g_object_set(renderer, "width", (gint)150, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_NAME));
    g_signal_connect(G_OBJECT(column), "clicked", G_CALLBACK(column_clicked), fields_vws->used_tree_view);
    
    /* Third row */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(GTK_TREE_VIEW(fields_vws->used_tree_view), column);
    gtk_tree_view_column_set_title(column, "Component");
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(column, renderer, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_NUM_COMP, NULL);
    g_object_set(renderer, "width", (gint)60, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_NUM_COMP));
    g_signal_connect(G_OBJECT(column), "clicked", G_CALLBACK(column_clicked), fields_vws->used_tree_view);
    
    /* Forth row */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(GTK_TREE_VIEW(fields_vws->used_tree_view), column);
    gtk_tree_view_column_set_title(column, "Field output");
    renderer = gtk_cell_renderer_toggle_new();
    g_signal_connect(G_OBJECT(renderer), "toggled", G_CALLBACK(toggle_viz_switch), fields_vws);
    gtk_tree_view_column_pack_start(column, renderer, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer, "active", COLUMN_VIZ_FLAG, NULL);
    g_object_set(renderer, "width", (gint)60, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_VIZ_FLAG));
    g_signal_connect(G_OBJECT(column), "clicked", G_CALLBACK(column_clicked), fields_vws->used_tree_view);
    
    /* Fifth row */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(GTK_TREE_VIEW(fields_vws->used_tree_view), column);
    gtk_tree_view_column_set_title(column, "Monitor output");
    renderer = gtk_cell_renderer_toggle_new();
    g_signal_connect(G_OBJECT(renderer), "toggled", G_CALLBACK(toggle_monitor_switch), fields_vws);
    gtk_tree_view_column_pack_start(column, renderer, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer, "active", COLUMN_MONITOR_FLAG, NULL);
    g_object_set(renderer, "width", (gint)60, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_MONITOR_FLAG));
    g_signal_connect(G_OBJECT(column), "clicked", G_CALLBACK(column_clicked), fields_vws->used_tree_view);
    
    /* 選択モード */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(fields_vws->used_tree_view));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* 1行毎に背景色を変更 */
    gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(fields_vws->used_tree_view), TRUE);
    
    /* ソート */
    column = gtk_tree_view_get_column(GTK_TREE_VIEW(fields_vws->used_tree_view), COLUMN_FIELD_INDEX);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_INDEX, GTK_SORT_ASCENDING);
    
    for(i=0;i<NUM_FIELD;i++){
		if(fields_vws->all_fld_tbl[i]->iflag_use > 0) {
			append_model_data(i, fields_vws->all_fld_tbl[i], child_model);
		};
    }
    
}

void create_unused_field_tree_view(struct field_views *fields_vws)
{
    /*    GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);*/
    GtkTreeModel *model;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    
    GtkListStore *child_model;
    
    int i;
    
    /* Construct empty list storage */
    child_model = gtk_list_store_new(7, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING,
                                     G_TYPE_INT, G_TYPE_INT, G_TYPE_INT, G_TYPE_INT);
    g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);
    
    /* ソート用のモデルを作成してツリービューにセットする */
    model = gtk_tree_model_sort_new_with_model(child_model);
    gtk_tree_view_set_model(GTK_TREE_VIEW(fields_vws->unused_field_tree_view), model);
    
    /* First raw */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(GTK_TREE_VIEW(fields_vws->unused_field_tree_view), column);
    gtk_tree_view_column_set_title(column, "Index");
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(column, renderer, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_FIELD_INDEX, NULL);
    g_object_set(renderer, "width", (gint)60, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_INDEX));
    g_signal_connect(G_OBJECT(column), "clicked", G_CALLBACK(column_clicked), 
                     fields_vws->unused_field_tree_view);
    
    /* Second row */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(GTK_TREE_VIEW(fields_vws->unused_field_tree_view), column);
    gtk_tree_view_column_set_title(column, "Field name");
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(column, renderer, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_FIELD_NAME, NULL);
    g_object_set(renderer, "width", (gint)150, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_NAME));
    g_signal_connect(G_OBJECT(column), "clicked", G_CALLBACK(column_clicked), fields_vws->unused_field_tree_view);
    
    /* 選択モード */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(fields_vws->unused_field_tree_view));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* 1行毎に背景色を変更 */
    gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(fields_vws->unused_field_tree_view), TRUE);
    
    /* ソート */
    column = gtk_tree_view_get_column(GTK_TREE_VIEW(fields_vws->unused_field_tree_view), COLUMN_FIELD_INDEX);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_INDEX, GTK_SORT_ASCENDING);
    
    for(i=0;i<NUM_FIELD;i++){
		if(fields_vws->all_fld_tbl[i]->iflag_use == 0) {
			append_model_data(i, fields_vws->all_fld_tbl[i], child_model);
		};
    }
    
}

void create_direction_tree_views(struct field_views *fields_vws)
{
	create_fixed_label_w_math_tree(fields_vws->scalar_label_view);
    append_scalar_componnet_label(fields_vws->scalar_label_view);
	
	create_fixed_label_w_math_tree(fields_vws->vector_label_view);
	append_vector_componnet_label(fields_vws->vector_label_view);
	
	create_fixed_label_w_math_tree(fields_vws->sym_tensor_label_view);
	append_vector_componnet_label(fields_vws->sym_tensor_label_view);
	
	create_fixed_label_w_math_tree(fields_vws->xyz_dir_label_view);
	append_vector_componnet_label(fields_vws->xyz_dir_label_view);
	
	create_fixed_label_w_math_tree(fields_vws->surface_eq_view);
	append_vector_componnet_label(fields_vws->surface_eq_view);
	
}

