/*
//  tree_views_4_fixed_lists_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#include "tree_views_4_fixed_lists_GTK.h"


GtkWidget * create_fixed_label_w_math_tree(void){
    /* Construct empty list storage */
    GtkListStore *child_model = gtk_list_store_new(3, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING);;
    /* Construct model for sorting and set to tree view */
    GtkTreeModel *model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    GtkWidget *label_tree = gtk_tree_view_new();
    
    gtk_tree_view_set_model(GTK_TREE_VIEW(label_tree), model);
    return label_tree;
}


GtkWidget * create_fixed_label_w_index_tree(void){
    /* Construct empty list storage */
    GtkListStore *child_model = gtk_list_store_new(3, G_TYPE_INT, G_TYPE_STRING, G_TYPE_INT);    
    /* Construct model for sorting and set to tree view */
    GtkTreeModel *model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    GtkWidget *label_tree = gtk_tree_view_new();

    gtk_tree_view_set_model(GTK_TREE_VIEW(label_tree), model);
    return label_tree;
}

void create_fixed_constant_tree(GtkWidget *label_tree)
{
    /*    GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);*/
    GtkTreeModel *model;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    
    GtkListStore *child_model;
    
    /* Construct empty list storage */
    child_model = gtk_list_store_new(4, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING,
                                     G_TYPE_DOUBLE);
    g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);
    
    /* Construct model for sorting and set to tree view */
    model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    gtk_tree_view_set_model(GTK_TREE_VIEW(label_tree), model);
    
    /* First raw */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(GTK_TREE_VIEW(label_tree), column);
    gtk_tree_view_column_set_title(column, "Index");
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(column, renderer, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_FIELD_INDEX, NULL);
    g_object_set(renderer, "width", (gint)60, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_INDEX));
    
    /* Second row */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(GTK_TREE_VIEW(label_tree), column);
    gtk_tree_view_column_set_title(column, "Field name");
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(column, renderer, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_FIELD_NAME, NULL);
    g_object_set(renderer, "width", (gint)150, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_NAME));
    
    /* Second row */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(GTK_TREE_VIEW(label_tree), column);
    gtk_tree_view_column_set_title(column, "value");
    renderer = gtk_cell_renderer_spin_new();
    gtk_tree_view_column_pack_start(column, renderer, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_FORTH, NULL);
    g_object_set(renderer, "width", (gint)150, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FORTH));
    
    
    /* 選択モード */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(label_tree));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* sort */
    column = gtk_tree_view_get_column(GTK_TREE_VIEW(label_tree), COLUMN_FIELD_INDEX);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_INDEX, GTK_SORT_ASCENDING);
}

void append_vector_componnet_label(struct flag_with_math_f *components_flag, GtkWidget *label_tree){
    GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree));  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    int i;
    GtkTreeIter iter;
    for(i=0;i<components_flag->num_flags;i++){
        gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
        gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                           COLUMN_FIELD_INDEX, i,
                           COLUMN_FIELD_NAME, components_flag->component_name[i],
                           COLUMN_FIELD_MATH, components_flag->component_math[i],
                           -1);
    }
    
}

void append_xyz_componnet_label(GtkWidget *label_tree){
    GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree));  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    int i;
    GtkTreeIter iter;
    for(i=0;i<NUM_XYZ_FLAG;i++){
        gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
        gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                           COLUMN_FIELD_INDEX, i,
                           COLUMN_FIELD_NAME, xyz_vector_flags[i].flag_name,
                           COLUMN_FIELD_MATH, xyz_vector_flags[i].flag_math,
                           -1);
    }
    
}

void append_surface_equation_label(GtkWidget *label_tree){
    GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree));  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    int i;
    GtkTreeIter iter;
    for(i=0;i<NTERM_PLANE;i++){
        gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
        gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                           COLUMN_FIELD_INDEX, i,
                           COLUMN_FIELD_NAME, surface_equation_flags[i].flag_name,
                           COLUMN_FIELD_MATH, surface_equation_flags[i].flag_math,
                           -1);
    }
    
}

void append_force_label(GtkWidget *label_tree){
    GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree));  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    int i;
    GtkTreeIter iter;
    for(i=0;i<NUM_TOTAL_FORCE;i++){
        gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
        gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                           COLUMN_FIELD_INDEX, i,
                           COLUMN_FIELD_NAME, force_flag_def[i].flag_name,
                           COLUMN_FIELD_MATH, force_flag_def[i].flag_math,
                           -1);
    }
    
}

void append_basic_force_label(GtkWidget *label_tree){
    GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree));  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    int i;
    GtkTreeIter iter;
    for(i=0;i<NUM_BASIC_FORCE;i++){
        gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
        gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                           COLUMN_FIELD_INDEX, i,
                           COLUMN_FIELD_NAME, force_flag_def[i].flag_name,
                           COLUMN_FIELD_MATH, force_flag_def[i].flag_math,
                           -1);
    }
    
}

void append_gravity_type_label(GtkWidget *label_tree){
    GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(label_tree));  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    int i;
    GtkTreeIter iter;
    for(i=0;i<NUM_GRAVITY_DEF;i++){
        gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
        gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                           COLUMN_FIELD_INDEX, i,
                           COLUMN_FIELD_NAME, gravity_type_def[i].flag_name,
                           COLUMN_FIELD_MATH, gravity_type_def[i].flag_math,
                           -1);
    }
    
}

