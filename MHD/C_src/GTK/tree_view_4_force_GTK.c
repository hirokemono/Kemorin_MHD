/*
//  tree_view_4_force_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#include "tree_view_4_force_GTK.h"


void init_dimless_views_GTK(struct mhd_model_control_c *model_ctl, struct dimless_views *dless_vws){
    dless_vws->dless_ctl_gtk = model_ctl->dless_ctl;
    return;
}

void dealloc_dimless_views_GTK(struct dimless_views *dless_vws){
    return;
}


/* Append new data at the end of list */

void append_default_coefs_label(GtkWidget *label_tree){
    GtkTreeModel *model = gtk_tree_view_get_model (label_tree);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    int i, j;
    GtkTreeIter iter;
    for(i=0;i<NUM_DEFAULT_COEF_DEF;i++){
        j = append_cr_item_to_tree(i, default_coefs_def[i].flag_name,
                            default_coefs_def[i].flag_math, 
                            default_coefs_def[i].value, child_model);
    }
    
}

static void dimless_name_edited_cb(GtkCellRendererText *cell, gchar *path_str, 
			gchar *new_text, gpointer user_data)
{
    struct dimless_views *dless_vws = (struct dimless_views *) user_data;
	
	cr_tree_name_edited(path_str, new_text, dless_vws->dimless_tree_view, 
				&dless_vws->dless_ctl_gtk->dimless_list);
	write_chara_real_ctl_list(stdout, 0, "dimless_test", 
                              &dless_vws->dless_ctl_gtk->dimless_list);
 
}
static void dimless_value_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data)
{
    struct dimless_views *dless_vws = (struct dimless_views *) user_data;
	
	cr_tree_value_edited(path_str, new_text, dless_vws->dimless_tree_view,
                    &dless_vws->dless_ctl_gtk->dimless_list);
    write_chara_real_ctl_list(stdout, 0, "dimless_test", 
                              &dless_vws->dless_ctl_gtk->dimless_list);
 
}

static void create_dimless_tree_view(struct dimless_views *dless_vws)
{
    GtkCellRenderer *renderer_text = gtk_cell_renderer_text_new();
    GtkCellRenderer *renderer_spin = gtk_cell_renderer_spin_new();
	
	create_text_real_tree_view(GTK_TREE_VIEW(dless_vws->dimless_tree_view),
				 renderer_text, renderer_spin);
	
	g_signal_connect(G_OBJECT(renderer_text), "edited", 
				G_CALLBACK(dimless_name_edited_cb), dless_vws);
	g_signal_connect(G_OBJECT(renderer_spin), "edited", 
				G_CALLBACK(dimless_value_edited_cb), dless_vws);
};
void init_dimless_tree_view(struct dimless_views *dless_vws){
    create_dimless_tree_view(dless_vws);
    dless_vws->index_dless = append_cr_list_from_ctl(dless_vws->index_dless, 
				&dless_vws->dless_ctl_gtk->dimless_list, dless_vws->dimless_tree_view);
}

void create_used_dimless_tree_views(struct dimless_views *dless_vws)
{
    create_fixed_constant_tree(dless_vws->default_dless_view);
    append_default_coefs_label(dless_vws->default_dless_view);
}


