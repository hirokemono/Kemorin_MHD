/*
//  tree_views_4_fixed_lists_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#include "tree_views_4_fixed_lists_GTK.h"

void set_last_field_to_label(GtkTreeSelection *selection, gpointer user_data)
{
    GtkLabel *label;
    GtkTreeModel *model;
    GList *list;
    GList *cur;
    gchar *row_string;
    GtkTreeIter iter;

    label = g_object_get_data(G_OBJECT(selection), "label");

    list = gtk_tree_selection_get_selected_rows(selection, &model);
    if (list == NULL) {
        gtk_label_set_text(label, "");
        return;
    }
    
    for (cur = g_list_first(list); cur != NULL; cur = g_list_next(cur)) {
        if (gtk_tree_model_get_iter(model, &iter, (GtkTreePath *)cur->data) == TRUE) {
            gtk_tree_model_get(model, &iter, COLUMN_FIELD_NAME, &row_string, -1);
        }
        gtk_tree_path_free((GtkTreePath *)cur->data);
    }
    g_list_free(list);
    
    gtk_label_set_text(label, row_string);
}



void block_changed_signal(GObject *instance)
{
	GList *list;
	GList *cur;
	gulong handler_id;
	GtkTreeSelection *selection;

	list = g_object_get_data(G_OBJECT(instance), "selection_list");
	for (cur = g_list_first(list); cur != NULL; cur = g_list_next(cur)) {
		selection = cur->data;
		handler_id = GPOINTER_TO_UINT(g_object_get_data(G_OBJECT(selection), "changed_handler_id"));
		g_signal_handler_block(G_OBJECT(selection), handler_id);
	}
}

void unblock_changed_signal(GObject *instance)
{
	GList *list;
	GList *cur;
	gulong handler_id;
	GtkTreeSelection *selection;
	
	list = g_object_get_data(G_OBJECT(instance), "selection_list");
	for (cur = g_list_first(list); cur != NULL; cur = g_list_next(cur)) {
		selection = cur->data;
		handler_id = GPOINTER_TO_UINT(g_object_get_data(G_OBJECT(selection), "changed_handler_id"));
		g_signal_handler_unblock(G_OBJECT(selection), handler_id);

		/* changedシグナルをブロックしていた間の変更を反映させる */
		set_last_field_to_label(selection, NULL);
	}
}


void add_sorting_shgnal_w_label(GtkTreeView *tree_view, GtkWidget *hbox){
    GtkTreeSelection *selection;
	GtkTreeModel *model;
    GtkTreeModel *child_model;
    gulong changed_handler_id;
    GtkWidget *label;
    GList *list;
	
    /*
     * selectionにchangedシグナルハンドラを登録する。
     * 後で同じchild_modelを使用しているselectionのchangedシグナルをブロック出来るように
     * child_modelにselectionのリストを、selectionにシグナルハンドラIDを登録する。
     * changedハンドラ内で使用するlabelも同様に登録しておく。
     */
    label = gtk_label_new("");
    gtk_box_pack_end(GTK_BOX(hbox), label, TRUE, TRUE, 0);
	
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view));
    changed_handler_id = g_signal_connect(G_OBJECT(selection), "changed",
                                          G_CALLBACK(set_last_field_to_label), NULL);
    g_object_set_data(G_OBJECT(selection), "changed_handler_id", GUINT_TO_POINTER(changed_handler_id));
    g_object_set_data(G_OBJECT(selection), "label", label);
    
    model = gtk_tree_view_get_model(GTK_TREE_VIEW(tree_view));
    child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    list = g_object_get_data(G_OBJECT(child_model), "selection_list");
    list = g_list_append(list, selection);
    g_object_set_data(G_OBJECT(child_model), "selection_list", list);
	return;
}



void create_fixed_label_w_math_tree(GtkWidget *label_tree){
    GtkTreeModel *model;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    
    GtkListStore *child_model;
    
    /* Construct empty list storage */
    child_model = gtk_list_store_new(3, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING);
    
    /* ソート用のモデルを作成してツリービューにセットする */
    model = gtk_tree_model_sort_new_with_model(child_model);
    gtk_tree_view_set_model(GTK_TREE_VIEW(label_tree), model);
}


void create_fixed_label_w_index_tree(GtkWidget *label_tree){
    GtkTreeModel *model;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    
    GtkListStore *child_model;
    
    /* Construct empty list storage */
    child_model = gtk_list_store_new(3, G_TYPE_INT, G_TYPE_STRING, G_TYPE_INT);
    
    /* ソート用のモデルを作成してツリービューにセットする */
    model = gtk_tree_model_sort_new_with_model(child_model);
    gtk_tree_view_set_model(GTK_TREE_VIEW(label_tree), model);
}

void create_fixed_constant_tree(GtkWidget *label_tree)
{
    /*    GtkTreeModel *child_model = GTK_TREE_MODEL(user_data);*/
    GtkTreeModel *model;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    GtkTreeSelection *selection;
    
    GtkListStore *child_model;
    
    int i;
    
    /* Construct empty list storage */
    child_model = gtk_list_store_new(4, G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING,
                                     G_TYPE_DOUBLE);
    g_object_set_data(G_OBJECT(child_model), "selection_list", NULL);
    
    /* ソート用のモデルを作成してツリービューにセットする */
    model = gtk_tree_model_sort_new_with_model(child_model);
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
    gtk_tree_view_column_set_attributes(column, renderer, "text", COLUMN_FIELD_VALUE, NULL);
    g_object_set(renderer, "width", (gint)150, NULL);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_clickable(column, TRUE);
    g_object_set_data(G_OBJECT(column), "column_id", GINT_TO_POINTER(COLUMN_FIELD_VALUE));
    
    
    /* 選択モード */
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(label_tree));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    
    /* 1行毎に背景色を変更 */
    gtk_tree_view_set_rules_hint(GTK_TREE_VIEW(label_tree), TRUE);
    
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

