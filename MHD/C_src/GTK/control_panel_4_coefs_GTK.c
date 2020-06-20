/*
//  control_panel_4_coefs_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/16.
*/

#include "control_panel_4_coefs_GTK.h"

static void delete_dless_data(gpointer user_data)
{
    struct momentum_coefs_view *coefs_vw = (struct momentum_coefs_view *) user_data;

    GtkTreeModel *model_to_del;
    GtkTreeModel *child_model_to_del;
    GtkTreeSelection *selection;
    GList *list;
    GList *reference_list;
    GList *cur;
    
    gchar *field_name;
    gchar *field_math;
    int index_field;
    double value;
    
    /* Get path of selected raw */
    /* The path is for tree_model_sort */
    model_to_del = gtk_tree_view_get_model(coefs_vw->coefs_tree_view);
    child_model_to_del = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_del));
    
    selection = gtk_tree_view_get_selection(coefs_vw->coefs_tree_view);
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
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_NAME, &field_name, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FIELD_MATH, &field_math, -1);
        gtk_tree_model_get(child_model_to_del, &iter, COLUMN_FORTH, &value, -1);
        
        printf("To be moved: %d, %s %lf\n", index_field, field_name, value);
        /* Delete */
        gtk_list_store_remove(GTK_LIST_STORE(child_model_to_del), &iter);
        
        gtk_tree_path_free(tree_path);
        gtk_tree_row_reference_free((GtkTreeRowReference *)cur->data);
        
        /* Update control data */
        del_chara_real_clist_by_c_tbl(field_name, coefs_vw->mom_ctl_gtk->coef_4_termal_buo_list);
    }
    g_list_free(reference_list);
    
    /* Release the block of changed signal */
    unblock_changed_signal(G_OBJECT(child_model_to_del));
}

static void remove_field_to_use(GtkButton *button, gpointer user_data)
{
    delete_dless_data(user_data);
    /*
     write_chara_real_ctl_list(stdout, 0, "Added list", 
     &coefs_vw->mom_ctl_gtk->coef_4_termal_buo_list);
     */
}

static void cb_set_dimless_name(GtkComboBox *combobox_field, gpointer user_data)
{
    struct momentum_coefs_view *coefs_vw = (struct momentum_coefs_view *) user_data;
    GtkTreeModel *model_field = gtk_combo_box_get_model(combobox_field);
    GtkTreeIter iter;
    
    gchar *row_string;
    int index_field;
    
    gint idx = gtk_combo_box_get_active(combobox_field);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_field, &iter, path);  
    gtk_tree_model_get(model_field, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_field, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    
    printf("Selected field %d, %s\n", index_field, row_string);
    
    return;
}

static void cb_add_dimless_name(GtkComboBox *combobox_add, gpointer user_data)
{
    struct momentum_coefs_view *coefs_vw = (struct momentum_coefs_view *) user_data;
    GtkTreeModel *model_to_add = gtk_tree_view_get_model(coefs_vw->coefs_tree_view);
    GtkTreeModel *child_model_to_add = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_to_add));
    
    GtkTreeModel *model_comp = gtk_combo_box_get_model(combobox_add);  
    
    GtkTreeIter iter;
    
    gchar *row_string;
    gchar *math_string;
    int index_comp;
    double value;
    
    gint idx = gtk_combo_box_get_active(combobox_add);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_comp, &iter, path);  
    gtk_tree_model_get(model_comp, &iter, COLUMN_FIELD_INDEX, &index_comp, -1);
    gtk_tree_model_get(model_comp, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_comp, &iter, COLUMN_FIELD_MATH, &math_string, -1);
    gtk_tree_model_get(model_comp, &iter, COLUMN_FORTH, &value, -1);
    
    append_chara_real_clist(row_string, value, 
							coefs_vw->mom_ctl_gtk->coef_4_termal_buo_list);
    /*
     write_chara_real_ctl_list(stdout, 0, "Added list", &
     coefs_vw->mom_ctl_gtk->coef_4_termal_buo_list);
     */
    return;
}

void add_coefs_selection_box(struct momentum_coefs_view *coefs_vw, GtkWidget *vbox)
{
    GtkWidget *hbox;
    GtkWidget *button;
    
    GtkTreeModel *model_comp;
    GtkWidget *combobox_add;
    GtkCellRenderer *column_add;
    
    GtkWidget *scrolled_window;
    
    char *c_label;
    
    c_label = (char *)calloc(KCHARA_C, sizeof(char));
    
    model_comp =  gtk_tree_view_get_model(coefs_vw->dimless_tree_view);
    
    hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
    
    /* Add data combocox */
    gtk_box_pack_start(GTK_BOX(hbox), gtk_label_new("Add: "), TRUE, TRUE, 0);
    combobox_add = gtk_combo_box_new_with_model(model_comp);
    g_signal_connect(G_OBJECT(combobox_add), "changed", 
                     G_CALLBACK(cb_add_dimless_name), (gpointer) coefs_vw);
    gtk_box_pack_start(GTK_BOX(hbox), combobox_add, FALSE, FALSE, 0);
    /* Delete data bottun */
    button = gtk_button_new_with_label("Remove");
    g_signal_connect(G_OBJECT(button), "clicked", 
                     G_CALLBACK(remove_field_to_use), (gpointer) coefs_vw);
    gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);
    
    column_add = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_add), column_add, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_add), column_add,
                                   "text", COLUMN_FIELD_NAME, NULL);
    
    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(scrolled_window, 400, 300);
    gtk_container_add(GTK_CONTAINER(scrolled_window), GTK_WIDGET(coefs_vw->coefs_tree_view));
    gtk_box_pack_start(GTK_BOX(vbox), scrolled_window, TRUE, TRUE, 0);
	
	add_sorting_signal_w_label(coefs_vw->coefs_tree_view, hbox);
};

