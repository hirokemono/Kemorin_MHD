/*
//  tree_view_4_each_term_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/16.
*/

#include "tree_view_4_each_term_GTK.h"


static void init_momentum_views_GTK(struct mhd_model_control_c *model_ctl,
                                    struct dimless_views *dless_vws, 
                                    struct momentum_coefs_view *mom_vws){
    mom_vws->mom_ctl_gtk = model_ctl->eqs_ctl->mom_ctl_c;
    return;
}

void init_coefs_views_GTK(struct mhd_model_control_c *model_ctl, struct coefs_view *coef_vws){
    coef_vws->dless_vws = (struct dimless_views *) malloc(sizeof(struct dimless_views));
    init_dimless_views_GTK(model_ctl, coef_vws->dless_vws);
    
    coef_vws->mom_vws = (struct momentum_coefs_view *) malloc(sizeof(struct momentum_coefs_view));
    init_momentum_views_GTK(model_ctl, coef_vws->dless_vws, coef_vws->mom_vws);
    return;
}

static void thermal_buo_name_edited_cb(GtkCellRendererText *cell, gchar *path_str, 
			gchar *new_text, gpointer user_data)
{
    struct momentum_coefs_view *mom_vws = (struct momentum_coefs_view *) user_data;
	
	cr_tree_name_edited(path_str, new_text, mom_vws->coefs_tree_view, 
				&mom_vws->mom_ctl_gtk->coef_4_termal_buo_list);
	write_chara_real_ctl_list(stdout, 0, "buoyancy changed", 
                              &mom_vws->mom_ctl_gtk->coef_4_termal_buo_list);
 
}
static void thermal_buo_value_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data)
{
    struct momentum_coefs_view *mom_vws = (struct momentum_coefs_view *) user_data;
	
	cr_tree_value_edited(path_str, new_text, mom_vws->coefs_tree_view,
                    &mom_vws->mom_ctl_gtk->coef_4_termal_buo_list);
    write_chara_real_ctl_list(stdout, 0, "buoyancy changed", 
                              &mom_vws->mom_ctl_gtk->coef_4_termal_buo_list);
 
}


static void cb_delete_thermal_buo_coef_new(GtkButton *button, gpointer user_data)
{
    struct momentum_coefs_view *mom_vws = (struct momentum_coefs_view *) user_data;
    
    delete_cr_list_items_GTK(mom_vws->coefs_tree_view, 
                             &mom_vws->mom_ctl_gtk->coef_4_termal_buo_list);
    write_chara_real_ctl_list(stdout, 0, "buoyancy coeffient deleted", 
                              &mom_vws->mom_ctl_gtk->coef_4_termal_buo_list);
    
}

static void cb_add_thermal_buo_coef_new(GtkButton *button, gpointer user_data)
{
    struct momentum_coefs_view *mom_vws = (struct momentum_coefs_view *) user_data;
    
    mom_vws->index_coefs = add_cr_list_by_bottun_GTK(mom_vws->index_coefs, 
                                                   mom_vws->coefs_tree_view,
                                                   &mom_vws->mom_ctl_gtk->coef_4_termal_buo_list);
    write_chara_real_ctl_list(stdout, 0, "buoyancy coeffient added", 
                              &mom_vws->mom_ctl_gtk->coef_4_termal_buo_list);
    return;
}

void init_momentum_tree_view(struct momentum_coefs_view *mom_vws){
    GtkCellRenderer *renderer_text = gtk_cell_renderer_text_new();
    GtkCellRenderer *renderer_spin = gtk_cell_renderer_spin_new();
    
    create_text_real_tree_view(GTK_TREE_VIEW(mom_vws->coefs_tree_view),
                               renderer_text, renderer_spin);
    
    g_signal_connect(G_OBJECT(renderer_text), "edited", 
                     G_CALLBACK(thermal_buo_name_edited_cb), mom_vws);
    g_signal_connect(G_OBJECT(renderer_spin), "edited", 
                     G_CALLBACK(thermal_buo_value_edited_cb), mom_vws);

    mom_vws->index_coefs = append_cr_list_from_ctl(mom_vws->index_coefs, 
                                                &mom_vws->mom_ctl_gtk->coef_4_termal_buo_list,
                                                mom_vws->coefs_tree_view);
}

void add_thermal_buo_selection_box(struct momentum_coefs_view *mom_vws, GtkWidget *vbox)
{
    GtkWidget *button_add = gtk_button_new_from_stock(GTK_STOCK_ADD);
    GtkWidget *button_delete = gtk_button_new_from_stock(GTK_STOCK_REMOVE);
	
	add_chara_real_list_box_w_addbottun(mom_vws->coefs_tree_view,
				button_add, button_delete, vbox);
	
    /* Add callbacks */
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(cb_add_thermal_buo_coef_new), mom_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(cb_delete_thermal_buo_coef_new), mom_vws);
};

