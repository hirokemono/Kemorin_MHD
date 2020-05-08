/*
//  tree_view_4_field_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/14.
*/

#include "tree_view_4_field_GTK.h"

struct field_gtk_data *  init_field_gtk_data(struct field_ctl_c *fld_ctl_ref){
	struct field_gtk_data *fld_gtk_data;
	if((fld_gtk_data = (struct field_gtk_data *) malloc(sizeof(struct field_gtk_data))) == NULL){
		printf("malloc error for field_gtk_data\n");
		exit(0);
	};
	
	fld_gtk_data->fld_ctl_gtk = fld_ctl_ref;
	fld_gtk_data->all_fld_list = init_all_field_ctl_c();
	load_field_w_qflag_from_ctl(fld_gtk_data->fld_ctl_gtk, fld_gtk_data->all_fld_list);
	return fld_gtk_data;
};

void dealloc_field_gtk_data(struct field_gtk_data *fld_gtk_data){
	dealloc_all_field_ctl_c(fld_gtk_data->all_fld_list);
	free(fld_gtk_data);
	return;
}

struct field_views * init_field_views_GTK(struct field_ctl_c *fld_ctl_ref){
    struct field_views *fields_vws;
	if((fields_vws = (struct field_views *) malloc(sizeof(struct field_views))) == NULL){
		printf("malloc error for field_views\n");
		exit(0);
	};
	fields_vws->fld_gtk_data = init_field_gtk_data(fld_ctl_ref);
	return fields_vws;
}

void dealloc_field_views_GTK(struct field_views *fields_vws){
	dealloc_field_gtk_data(fields_vws->fld_gtk_data);
	free(fields_vws);
	return;
}


/* Append new data at the end of list */
void append_field_model_data(int index_field, struct all_field_ctl_c *all_fld_list, 
			GtkListStore *child_model)
{
    GtkTreeIter iter;
    
    gtk_list_store_append(child_model, &iter);
    gtk_list_store_set(child_model, &iter,
                       COLUMN_FIELD_INDEX, index_field,
                       COLUMN_FIELD_NAME, all_fld_list->fld_list->field_name[index_field],
                       COLUMN_FIELD_MATH, all_fld_list->fld_list->field_math[index_field],
                       COLUMN_FORTH, (gboolean) all_fld_list->iflag_viz[index_field],
                       COLUMN_FIFTH, (gboolean) all_fld_list->iflag_monitor[index_field],
                       COLUMN_NUM_COMP, all_fld_list->fld_list->num_comp[index_field],
                       COLUMN_QUADRATURE, all_fld_list->iflag_quad[index_field],
                       -1);
}

static void toggle_viz_switch(GtkTreeViewColumn *renderer, 
			gchar *path_str, gpointer user_data){
	GtkWidget *used_tree_view = GTK_WIDGET(user_data);
	struct field_gtk_data *fld_gtk_data
			= (struct field_gtk_data *) g_object_get_data(G_OBJECT(used_tree_view), "field_gtk");
    GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(GTK_TREE_VIEW(used_tree_view)));  
	GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;

    gchar *row_string;
    int index_field, index_for_toggle;

    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FORTH, &index_for_toggle, -1);
    
   
    printf("toggle_viz_switch %d, %s: %s\n", index_field, row_string,
            fld_gtk_data->all_fld_list->fld_list->field_name[index_field]);
    
    index_for_toggle = (index_for_toggle+ 1) % 2;
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FORTH, (gboolean) index_for_toggle, -1);
    gtk_tree_path_free(child_path);  
    gtk_tree_path_free(path);  
    
    fld_gtk_data->all_fld_list->iflag_viz[index_field] = index_for_toggle;
	update_field_flag_wqflag_in_ctl(index_field, fld_gtk_data->all_fld_list, fld_gtk_data->fld_ctl_gtk);
}

static void toggle_monitor_switch(GtkTreeViewColumn *renderer, gchar *path_str, gpointer user_data){
	GtkWidget *used_tree_view = GTK_WIDGET(user_data);
	struct field_gtk_data *fld_gtk_data
			= (struct field_gtk_data *) g_object_get_data(G_OBJECT(used_tree_view), "field_gtk");
	
	GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(used_tree_view));  
	GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
	GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
	GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;

    gchar *row_string;
    int index_field, index_for_toggle;
    
    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &row_string, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_FIFTH, &index_for_toggle, -1);
    
    printf("toggle_monitor_switch %d, %s: %s\n", index_field, row_string,
           fld_gtk_data->all_fld_list->fld_list->field_name[index_field]);
	
    index_for_toggle = (index_for_toggle+ 1) % 2;
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIFTH, (gboolean) index_for_toggle, -1);
    gtk_tree_path_free(child_path);  
    gtk_tree_path_free(path);  
    
    fld_gtk_data->all_fld_list->iflag_monitor[index_field] = index_for_toggle;
	update_field_flag_wqflag_in_ctl(index_field, fld_gtk_data->all_fld_list, fld_gtk_data->fld_ctl_gtk);
}


static void create_field_tree_columns(GtkWidget *used_tree_view, struct field_gtk_data *fld_gtk_data)
{
    GtkCellRenderer *textRenderer1;
    GtkCellRenderer *textRenderer2;
    GtkCellRenderer *textRenderer3;
    GtkCellRenderer *toggleRenderer1;
    GtkCellRenderer *toggleRenderer2;
	
	GtkTreeViewColumn *column_1st;
    GtkTreeViewColumn *column_2nd;
    GtkTreeViewColumn *column_3rd;
    GtkTreeViewColumn *column_4th;
    GtkTreeViewColumn *column_5th;
	
	g_object_set_data(G_OBJECT(used_tree_view), "field_gtk", fld_gtk_data);
	
	/* First raw */
	column_1st = create_each_field_column(used_tree_view, "Index", COLUMN_FIELD_INDEX);
	textRenderer1 = create_each_text_renderer(column_1st, 60, COLUMN_FIELD_INDEX);
    
    /* Second row */
	column_2nd = create_each_field_column(used_tree_view, "Field name", COLUMN_FIELD_NAME);
	textRenderer2 = create_each_text_renderer(column_2nd, 180, COLUMN_FIELD_NAME);
   
    /* Third row */
	column_3rd = create_each_field_column(used_tree_view, "Component", COLUMN_NUM_COMP);
	textRenderer3 = create_each_text_renderer(column_3rd, 60, COLUMN_NUM_COMP);
    
    /* Forth row */
	column_4th = create_each_field_column(used_tree_view, "Field output", COLUMN_FORTH);
	toggleRenderer1 = create_each_toggle_renderer(column_4th, 60, COLUMN_FORTH);
	g_signal_connect(G_OBJECT(toggleRenderer1), "toggled", 
				G_CALLBACK(toggle_viz_switch), (gpointer) used_tree_view);
    
    /* Fifth row */
	column_5th = create_each_field_column(used_tree_view, "Monitor output", COLUMN_FIFTH);
	toggleRenderer2 = create_each_toggle_renderer(column_5th, 60, COLUMN_FIFTH);
	g_signal_connect(G_OBJECT(toggleRenderer2), "toggled",
				G_CALLBACK(toggle_monitor_switch), (gpointer) used_tree_view);
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
	
	create_field_tree_columns(fields_vws->used_tree_view, fields_vws->fld_gtk_data);
    
    /* 選択モード */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(fields_vws->used_tree_view));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
    column = gtk_tree_view_get_column(GTK_TREE_VIEW(fields_vws->used_tree_view), COLUMN_FIELD_INDEX);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_INDEX, GTK_SORT_ASCENDING);
    
    for(i=0;i<fields_vws->fld_gtk_data->all_fld_list->fld_list->ntot_fields;i++){
		if(fields_vws->fld_gtk_data->all_fld_list->iflag_use[i] > 0) {
			append_field_model_data(i, fields_vws->fld_gtk_data->all_fld_list, child_model);
		};
    }
    
}

void create_unused_field_tree_view(struct field_views *fields_vws)
{
    /*    GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);*/
    GtkTreeModel *model;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    
    GtkCellRenderer *textRenderer1;
    GtkCellRenderer *textRenderer2;
	
	GtkTreeViewColumn *column_1st;
	GtkTreeViewColumn *column_2nd;
	
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
	column_1st = create_each_field_column(fields_vws->unused_field_tree_view, "Index name", COLUMN_FIELD_INDEX);
	textRenderer1 = create_each_text_renderer(column_1st, 60, COLUMN_FIELD_INDEX);
	
	/* Second row */
	column_2nd = create_each_field_column(fields_vws->unused_field_tree_view, "Field name", COLUMN_FIELD_NAME);
	textRenderer2 = create_each_text_renderer(column_2nd, 180, COLUMN_FIELD_NAME);
    
    /* 選択モード */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(fields_vws->unused_field_tree_view));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
    column = gtk_tree_view_get_column(GTK_TREE_VIEW(fields_vws->unused_field_tree_view), COLUMN_FIELD_INDEX);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_INDEX, GTK_SORT_ASCENDING);
    
    for(i=0;i<fields_vws->fld_gtk_data->all_fld_list->fld_list->ntot_fields;i++){
		if(fields_vws->fld_gtk_data->all_fld_list->iflag_use[i] == 0) {
			append_field_model_data(i, fields_vws->fld_gtk_data->all_fld_list, child_model);
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

