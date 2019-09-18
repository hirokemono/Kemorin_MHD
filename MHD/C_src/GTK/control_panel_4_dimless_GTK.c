/*
//  control_panel_4_dimless_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#include "control_panel_4_dimless_GTK.h"


static void cb_set_dimless_name(GtkComboBox *combobox_field, gpointer user_data)
{
    struct dimless_views *dless_vws = (struct dimless_views *) user_data;
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



static void cb_deleta_dimless_lists(GtkButton *button, gpointer user_data)
{
    struct dimless_views *dless_vws = (struct dimless_views *) user_data;
    
    delete_cr_list_items_GTK(GTK_TREE_VIEW(dless_vws->dimless_tree_view), dless_vws->cr_clist);
    write_chara_real_clist(stdout, 0, "Added dimless list", dless_vws->cr_clist);
    
}

static void cb_add_dimless_new(GtkButton *button, gpointer user_data)
{
    struct dimless_views *dless_vws = (struct dimless_views *) user_data;
/*
    dless_vws->index_dless = add_cr_list_by_bottun_GTK(dless_vws->index_dless, 
                                                   dless_vws->dimless_tree_view,
                                                   dless_vws->cr_clist);
 */
    dless_vws->index_dless = add_cr_list_items_GTK(GTK_TREE_VIEW(dless_vws->dimless_tree_view),
                                                   dless_vws->cr_clist);
    write_chara_real_clist(stdout, 0, "Added list", dless_vws->cr_clist);
    return;
}

static void cb_add_dimless_name(GtkComboBox *combobox_add, gpointer user_data)
{
    struct dimless_views *dless_vws = (struct dimless_views *) user_data;
    GtkTreeModel *model_comp = gtk_combo_box_get_model(combobox_add);  
	
    gint idx = gtk_combo_box_get_active(combobox_add);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
	dless_vws->index_dless = add_cr_list_from_combobox_GTK(dless_vws->index_dless, 
				path, model_comp, GTK_TREE_VIEW(dless_vws->dimless_tree_view), dless_vws->cr_clist);
    write_chara_real_clist(stdout, 0, "Added list", dless_vws->cr_clist);

    return;
}


void add_dimless_selection_box(struct dimless_views *dless_vws, GtkWidget *vbox)
{
    GtkTreeModel *model_default =  gtk_tree_view_get_model(GTK_TREE_VIEW(dless_vws->default_dless_view));
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *combobox_add = gtk_combo_box_new_with_model(model_default);
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	
	add_chara_real_list_box_w_combobox(GTK_TREE_VIEW(dless_vws->dimless_tree_view),
				button_add, combobox_add, button_delete, vbox);
	
    /* Add callbacks */
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(cb_add_dimless_new), (gpointer) dless_vws);
    g_signal_connect(G_OBJECT(combobox_add), "changed", 
                     G_CALLBACK(cb_add_dimless_name), (gpointer) dless_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(cb_deleta_dimless_lists), (gpointer) dless_vws);
};

void add_dimless_combobox_vbox(struct dimless_views *dless_vws, GtkWidget *vbox)
{
    GtkTreeModel *model_field;
    GtkWidget *hbox;
    GtkWidget *combobox_field;
    GtkCellRenderer *column_field;
    
    model_field = gtk_tree_view_get_model(GTK_TREE_VIEW(dless_vws->dimless_tree_view));
    
    hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
    combobox_field = gtk_combo_box_new_with_model(model_field);
    g_signal_connect(G_OBJECT(combobox_field), "changed", 
                     G_CALLBACK(cb_set_dimless_name), (gpointer) dless_vws);
    column_field = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_field), column_field, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_field), column_field,
                                   "text", COLUMN_FIELD_NAME, NULL);
    gtk_box_pack_start(GTK_BOX(hbox), combobox_field, FALSE, FALSE, 0);

    
    gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
}
