/*
//  tree_view_4_field_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/14.
*/

#include "tree_view_4_field_GTK.h"

struct field_views * init_field_views_GTK(struct field_ctl_c *fld_ctl_ref){
    struct field_views *fields_vws = (struct field_views *) malloc(sizeof(struct field_views));
    fields_vws->fld_ctl_gtk = fld_ctl_ref;
    fields_vws->all_fld_tbl = (struct all_field_ctl_c **) malloc(NUM_FIELD * sizeof(struct all_field_ctl_c *));
    alloc_all_field_ctl_c(fields_vws->all_fld_tbl);
    load_field_w_qflag_from_ctl(fields_vws->fld_ctl_gtk, fields_vws->all_fld_tbl);
    return fields_vws;
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

static GtkTreeViewColumn * create_each_field_column(GtkWidget *tree_view,
			const char *label, int column_index)
{
    GtkTreeViewColumn *column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(GTK_TREE_VIEW(tree_view), column);
    gtk_tree_view_column_set_title(column, label);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(column_index));
	g_signal_connect(G_OBJECT(column), "clicked", G_CALLBACK(column_clicked), tree_view);
	return column;
};

static GtkCellRenderer * create_each_text_renderer(GtkTreeViewColumn *column,
			int iwidth, int column_index){
	GtkCellRenderer *renderer = gtk_cell_renderer_text_new();
	gtk_tree_view_column_pack_start(column, renderer, TRUE);
	gtk_tree_view_column_set_attributes(column, renderer, "text", column_index, NULL);
	g_object_set(renderer, "width", (gint) iwidth, NULL);
	return renderer;
};
static GtkCellRenderer * create_each_toggle_renderer(GtkTreeViewColumn *column, 
			int iwidth, int column_index, struct field_views *fields_vws){
	GtkCellRenderer *renderer = gtk_cell_renderer_toggle_new();
    gtk_tree_view_column_pack_start(column, renderer, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer, "active", column_index, NULL);
    g_object_set(renderer, "width", iwidth, NULL);
	return renderer;
};

void create_field_tree_columns(struct field_views *fields_vws)
{
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
	
    /* First raw */
	column = create_each_field_column(fields_vws->used_tree_view, 
				"Index", COLUMN_FIELD_INDEX);
	renderer = create_each_text_renderer(column, 60, COLUMN_FIELD_INDEX);
    
    /* Second row */
	column = create_each_field_column(fields_vws->used_tree_view, 
				"Field name", COLUMN_FIELD_NAME);
	renderer = create_each_text_renderer(column, 180, COLUMN_FIELD_NAME);
   
    /* Third row */
	column = create_each_field_column(fields_vws->used_tree_view,
				"Component", COLUMN_NUM_COMP);
	renderer = create_each_text_renderer(column, 60, COLUMN_NUM_COMP);
    
    /* Forth row */
	column = create_each_field_column(fields_vws->used_tree_view,
				"Field output", COLUMN_VIZ_FLAG);
	renderer = create_each_toggle_renderer(column, 60, COLUMN_VIZ_FLAG, fields_vws);
    g_signal_connect(G_OBJECT(renderer), "toggled", G_CALLBACK(toggle_viz_switch), fields_vws);
    
    /* Fifth row */
	column = create_each_field_column(fields_vws->used_tree_view,
				"Monitor output", COLUMN_MONITOR_FLAG);
	renderer = create_each_toggle_renderer(column, 60, COLUMN_MONITOR_FLAG, fields_vws);
    g_signal_connect(G_OBJECT(renderer), "toggled", G_CALLBACK(toggle_monitor_switch), fields_vws);
};

void create_field_tree_view(struct field_views *fields_vws)
{
    int i;
    GtkTreeModel *model;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    
    /* Construct empty list storage */
    GtkListStore *child_model = gtk_list_store_new(7, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING,
                                     G_TYPE_INT, G_TYPE_INT, G_TYPE_INT, G_TYPE_INT);
    g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);
    
    /* Construct model for sorting and set to tree view */
    fields_vws->used_tree_view = gtk_tree_view_new();
    model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    gtk_tree_view_set_model(GTK_TREE_VIEW(fields_vws->used_tree_view), model);
	
	create_field_tree_columns(fields_vws);
    
    /* 選択モード */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(fields_vws->used_tree_view));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
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
    
    /* Construct model for sorting and set to tree view */
    fields_vws->unused_field_tree_view = gtk_tree_view_new();
    model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    gtk_tree_view_set_model(GTK_TREE_VIEW(fields_vws->unused_field_tree_view), model);
    
    /* First raw */
	create_each_field_column(fields_vws->unused_field_tree_view, "Index name", COLUMN_FIELD_INDEX);
	create_each_text_renderer(column, 60, COLUMN_FIELD_INDEX);
	
	/* Second row */
	create_each_field_column(fields_vws->unused_field_tree_view, "Field name", COLUMN_FIELD_NAME);
	create_each_text_renderer(column, 180, COLUMN_FIELD_NAME);
    
    /* 選択モード */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(fields_vws->unused_field_tree_view));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
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
    fields_vws->scalar_label_view = create_fixed_label_w_math_tree();
    append_scalar_componnet_label(fields_vws->scalar_label_view);
	
	fields_vws->vector_label_view = create_fixed_label_w_math_tree();
	append_vector_componnet_label(fields_vws->vector_label_view);
	
	fields_vws->sym_tensor_label_view = create_fixed_label_w_math_tree();
	append_vector_componnet_label(fields_vws->sym_tensor_label_view);
	
	fields_vws->xyz_dir_label_view = create_fixed_label_w_math_tree();
	append_vector_componnet_label(fields_vws->xyz_dir_label_view);
	
	fields_vws->surface_eq_view = create_fixed_label_w_math_tree();
	append_vector_componnet_label(fields_vws->surface_eq_view);
	
}

