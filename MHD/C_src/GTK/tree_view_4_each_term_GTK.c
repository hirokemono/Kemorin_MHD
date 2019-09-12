/*
//  tree_view_4_each_term_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/16.
*/

#include "tree_view_4_each_term_GTK.h"


void init_momentum_views_GTK(struct chara_real_clist *cr_clist,
                                    struct dimless_views *dless_vws, 
                                    struct momentum_coefs_view *mom_vws){
    mom_vws->mom_ctl_gtk->coef_4_termal_buo_list = cr_clist;
    return;
}

void init_coefs_views_GTK(struct mhd_model_control_c *model_ctl, struct coefs_view *coef_vws){
    coef_vws->dless_vws = (struct dimless_views *) malloc(sizeof(struct dimless_views));
    init_dimless_views_GTK(model_ctl->dless_ctl->dimless_list, coef_vws->dless_vws);
    
    coef_vws->mom_vws = (struct momentum_coefs_view *) malloc(sizeof(struct momentum_coefs_view));
    init_momentum_views_GTK(model_ctl->eqs_ctl->mom_ctl_c->coef_4_termal_buo_list,
                            coef_vws->dless_vws, coef_vws->mom_vws);
    return;
}

static void thermal_buo_name_edited_cb(GtkCellRendererText *cell, gchar *path_str, 
			gchar *new_text, gpointer user_data)
{
    struct momentum_coefs_view *mom_vws = (struct momentum_coefs_view *) user_data;
	
	cr_tree_name_edited(path_str, new_text, mom_vws->coefs_tree_view, mom_vws->mom_ctl_gtk->coef_4_termal_buo_list);
	write_chara_real_clist(stdout, 0, "buoyancy changed", mom_vws->mom_ctl_gtk->coef_4_termal_buo_list);
}

static void thermal_buo_value_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data)
{
    struct momentum_coefs_view *mom_vws = (struct momentum_coefs_view *) user_data;
	
	cr_tree_value_edited(path_str, new_text, mom_vws->coefs_tree_view, mom_vws->mom_ctl_gtk->coef_4_termal_buo_list);
    write_chara_real_clist(stdout, 0, "buoyancy changed", mom_vws->mom_ctl_gtk->coef_4_termal_buo_list);
 
}


static void cb_delete_thermal_buo_coef_new(GtkButton *button, gpointer user_data)
{
    struct momentum_coefs_view *mom_vws = (struct momentum_coefs_view *) user_data;
    
    delete_cr_list_items_GTK(mom_vws->coefs_tree_view, mom_vws->mom_ctl_gtk->coef_4_termal_buo_list);
    write_chara_real_clist(stdout, 0, "buoyancy coeffient deleted", mom_vws->mom_ctl_gtk->coef_4_termal_buo_list);
    
}


static void cb_add_thermal_buo_coef(GtkComboBox *combobox_add, gpointer user_data)
{
    struct momentum_coefs_view *mom_vws = (struct momentum_coefs_view *) user_data;
    GtkTreeModel *model_comp = gtk_combo_box_get_model(combobox_add);  
    
    gint idx = gtk_combo_box_get_active(combobox_add);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    mom_vws->index_coefs = add_cr_list_from_combobox_GTK_w_one(mom_vws->index_coefs, 
                                                           path, model_comp, mom_vws->coefs_tree_view,
                                                           mom_vws->mom_ctl_gtk->coef_4_termal_buo_list);
    write_chara_real_clist(stdout, 0, "buoyancy coeffient added", mom_vws->mom_ctl_gtk->coef_4_termal_buo_list);
    return;
}

static void cb_add_thermal_buo_coef_new(GtkButton *button, gpointer user_data)
{
    struct momentum_coefs_view *mom_vws = (struct momentum_coefs_view *) user_data;
    
    mom_vws->index_coefs = add_cr_list_by_bottun_GTK(mom_vws->index_coefs, 
                                                   mom_vws->coefs_tree_view,
                                                   mom_vws->mom_ctl_gtk->coef_4_termal_buo_list);
    write_chara_real_clist(stdout, 0, "buoyancy coeffient added", mom_vws->mom_ctl_gtk->coef_4_termal_buo_list);
    return;
}

void init_momentum_tree_view(struct momentum_coefs_view *mom_vws){
    GtkCellRenderer *renderer_text = gtk_cell_renderer_text_new();
    GtkCellRenderer *renderer_spin = gtk_cell_renderer_spin_new();
    
    create_text_real_tree_view(mom_vws->mom_ctl_gtk->coef_4_termal_buo_list, 
                               GTK_TREE_VIEW(mom_vws->coefs_tree_view),
                               renderer_text, renderer_spin);
    
    g_signal_connect(G_OBJECT(renderer_text), "edited", 
                     G_CALLBACK(thermal_buo_name_edited_cb), (gpointer) mom_vws);
    g_signal_connect(G_OBJECT(renderer_spin), "edited", 
                     G_CALLBACK(thermal_buo_value_edited_cb), (gpointer) mom_vws);

    mom_vws->index_coefs = append_cr_list_from_ctl(mom_vws->index_coefs, 
                                                   &mom_vws->mom_ctl_gtk->coef_4_termal_buo_list->cr_item_head,
												   mom_vws->coefs_tree_view);
}

void add_thermal_buo_selection_box(struct momentum_coefs_view *mom_vws, GtkWidget *vbox)
{
    GtkTreeModel *model_default =  gtk_tree_view_get_model(GTK_TREE_VIEW(mom_vws->dimless_tree_view));
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *combobox_add = gtk_combo_box_new_with_model(model_default);
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	
	add_chara_real_list_box_w_combobox(mom_vws->coefs_tree_view,
				button_add, combobox_add, button_delete, vbox);
	
    /* Add callbacks */
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(cb_add_thermal_buo_coef_new), (gpointer) mom_vws);
    g_signal_connect(G_OBJECT(combobox_add), "changed", 
                     G_CALLBACK(cb_add_thermal_buo_coef), (gpointer) mom_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(cb_delete_thermal_buo_coef_new), (gpointer) mom_vws);
};

