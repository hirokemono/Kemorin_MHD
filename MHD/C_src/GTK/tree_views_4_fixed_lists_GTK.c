/*
//  tree_views_4_fixed_lists_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#include "tree_views_4_fixed_lists_GTK.h"


void create_fixed_label_tree(GtkWidget *label_tree){
    GtkTreeModel *model;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    
    GtkListStore *child_model;
    
    /* Construct empty list storage */
    child_model = gtk_list_store_new(3, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING);
    
    /* ソート用のモデルを作成してツリービューにセットする */
    model = gtk_tree_model_sort_new_with_model(child_model);
    gtk_tree_view_set_model(GTK_TREE_VIEW(label_tree), model);
    
    /* First raw */
    column = gtk_tree_view_column_new();
    gtk_tree_view_append_column(GTK_TREE_VIEW(label_tree), column);
    gtk_tree_view_column_set_title(column, "Fixed labels");
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(column, renderer, TRUE);
    gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_FIELD_INDEX, NULL);
    /*    g_object_set(renderer, "width", (gint)60, NULL);
     gtk_tree_view_column_set_resizable(column, FALSE);*/
    gtk_tree_view_column_set_clickable(column, FALSE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_INDEX));
    
    /* ソート */
    column = gtk_tree_view_get_column(GTK_TREE_VIEW(label_tree), COLUMN_FIELD_INDEX);
    gtk_tree_view_column_set_sort_order(column, GTK_SORT_ASCENDING);
    gtk_tree_view_column_set_sort_indicator(column, TRUE);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(model), COLUMN_FIELD_INDEX, GTK_SORT_ASCENDING);
}


void append_scalar_componnet_label(GtkWidget *label_tree){
    GtkTreeModel *model = gtk_tree_view_get_model (label_tree);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    int i;
    GtkTreeIter iter;
    for(i=0;i<NUM_SCALAR_FLAG;i++){
        gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
        gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                           COLUMN_FIELD_INDEX, i,
                           COLUMN_FIELD_NAME, scalar_flags[i].flag_name,
                           COLUMN_FIELD_MATH, scalar_flags[i].flag_math,
                           -1);
    }
    
}

void append_vector_componnet_label(GtkWidget *label_tree){
    GtkTreeModel *model = gtk_tree_view_get_model (label_tree);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    int i;
    GtkTreeIter iter;
    for(i=0;i<NUM_VECTOR_FLAG;i++){
        gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
        gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                           COLUMN_FIELD_INDEX, i,
                           COLUMN_FIELD_NAME, vector_flags[i].flag_name,
                           COLUMN_FIELD_MATH, vector_flags[i].flag_math,
                           -1);
    }
    
}

void append_sym_tensor_componnet_label(GtkWidget *label_tree){
    GtkTreeModel *model = gtk_tree_view_get_model (label_tree);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    int i;
    GtkTreeIter iter;
    for(i=0;i<NUM_SYM_TENSOR_FLAG;i++){
        gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
        gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                           COLUMN_FIELD_INDEX, i,
                           COLUMN_FIELD_NAME, sym_tensor_flags[i].flag_name,
                           COLUMN_FIELD_MATH, sym_tensor_flags[i].flag_math,
                           -1);
    }
    
}
void append_xyz_componnet_label(GtkWidget *label_tree){
    GtkTreeModel *model = gtk_tree_view_get_model (label_tree);  
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
    GtkTreeModel *model = gtk_tree_view_get_model (label_tree);  
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
    GtkTreeModel *model = gtk_tree_view_get_model (label_tree);  
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
    GtkTreeModel *model = gtk_tree_view_get_model (label_tree);  
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

void append_defaule_coefs_label(GtkWidget *label_tree){
    GtkTreeModel *model = gtk_tree_view_get_model (label_tree);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    int i;
    GtkTreeIter iter;
    for(i=0;i<NUM_DEFAULT_COEF_DEF;i++){
        gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
        gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                           COLUMN_FIELD_INDEX, i,
                           COLUMN_FIELD_NAME, default_coefs_def[i].flag_name,
                           COLUMN_FIELD_MATH, default_coefs_def[i].flag_math,
                           -1);
    }
    
}

void append_gravity_type_label(GtkWidget *label_tree){
    GtkTreeModel *model = gtk_tree_view_get_model (label_tree);  
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

