/*
//  tree_view_4_temp_BC_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/16.
*/

#include "tree_view_4_temp_BC_GTK.h"


void init_temp_bc_views_GTK(struct chara2_real_clist *bc_T_ctl,
                                    struct boundary_condition_view *bc_vws){
    bc_vws->bc_T_gtk = bc_T_ctl;
    return;
}

static void thermal_bc_position_edited_cb(GtkCellRendererText *cell, gchar *path_str, 
			gchar *new_text, gpointer user_data)
{
    struct boundary_condition_view *bc_vws = (struct boundary_condition_view *) user_data;
	
	c2r_tree_2nd_text_edited(path_str, new_text, bc_vws->bc_tree_view, bc_vws->bc_T_gtk);
	write_chara2_real_clist(stdout, 0, "bounday location changed", bc_vws->bc_T_gtk);
 
}
static void thermal_bc_type_edited_cb(GtkCellRendererText *cell, gchar *path_str, 
			gchar *new_text, gpointer user_data)
{
    struct boundary_condition_view *bc_vws = (struct boundary_condition_view *) user_data;
	
	c2r_tree_1st_text_edited(path_str, new_text, bc_vws->bc_tree_view, bc_vws->bc_T_gtk);
	write_chara2_real_clist(stdout, 0, "BC type changed", bc_vws->bc_T_gtk);
 
}
static void thermal_bc_value_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data)
{
    struct boundary_condition_view *bc_vws = (struct boundary_condition_view *) user_data;
	
	c2r_tree_value_edited(path_str, new_text, bc_vws->bc_tree_view, bc_vws->bc_T_gtk);
    write_chara2_real_clist(stdout, 0, "buoyancy changed", bc_vws->bc_T_gtk);
 
}


static void cb_delete_thermal_bc_by_list(GtkButton *button, gpointer user_data)
{
    struct boundary_condition_view *bc_vws = (struct boundary_condition_view *) user_data;
    
    delete_c2r_list_items_GTK(bc_vws->bc_tree_view, bc_vws->bc_T_gtk);
    write_chara2_real_clist(stdout, 0, "buoyancy coeffient deleted", bc_vws->bc_T_gtk);
    
}


static void cb_add_thermal_bc_by_list(GtkComboBox *combobox_add, gpointer user_data)
{
    struct boundary_condition_view *bc_vws = (struct boundary_condition_view *) user_data;
    GtkTreeModel *model_comp = gtk_combo_box_get_model(combobox_add);  
    
    gint idx = gtk_combo_box_get_active(combobox_add);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    bc_vws->index_bc = add_c2r_list_from_combobox_GTK_w_one(bc_vws->index_bc, path, model_comp, 
                                                            bc_vws->bc_tree_view, bc_vws->bc_T_gtk);
    write_chara2_real_clist(stdout, 0, "buoyancy coeffient added", bc_vws->bc_T_gtk);
    return;
}

static void cb_add_thermal_bc(GtkButton *button, gpointer user_data)
{
    struct boundary_condition_view *bc_vws = (struct boundary_condition_view *) user_data;
    
    bc_vws->index_bc = add_c2r_list_by_bottun_GTK(bc_vws->index_bc, 
                                                  bc_vws->bc_tree_view, bc_vws->bc_T_gtk);
    write_chara2_real_clist(stdout, 0, "buoyancy coeffient added", bc_vws->bc_T_gtk);
    return;
}

void init_bc_temp_tree_view(struct boundary_condition_view *bc_vws){
    GtkCellRenderer *renderer_cbox = gtk_cell_renderer_combo_new();
    GtkCellRenderer *renderer_text = gtk_cell_renderer_text_new();
    GtkCellRenderer *renderer_spin = gtk_cell_renderer_spin_new();
    
    GtkListStore *cbox_child_model = gtk_list_store_new(1, G_TYPE_STRING);
	
    int i;
    int index = 0;
    
	create_cbox_text_real_tree_view(cbox_child_model, bc_vws->bc_tree_view,
                renderer_cbox, renderer_text, renderer_spin);
    
    g_signal_connect(G_OBJECT(renderer_cbox), "edited", 
                     G_CALLBACK(thermal_bc_type_edited_cb), (gpointer) bc_vws);
    g_signal_connect(G_OBJECT(renderer_text), "edited", 
                     G_CALLBACK(thermal_bc_position_edited_cb), (gpointer) bc_vws);
    g_signal_connect(G_OBJECT(renderer_spin), "edited", 
                     G_CALLBACK(thermal_bc_value_edited_cb), (gpointer) bc_vws);

    bc_vws->index_bc = append_c2r_list_from_ctl(bc_vws->index_bc, &bc_vws->bc_T_gtk->c2r_item_head,
                                                bc_vws->bc_tree_view);
    for(i=0;i<NUM_BASIC_BC_TYPE_DEF;i++){
        index = append_c2r_item_to_tree(index, boundary_type_def[i], " ", ZERO, cbox_child_model);
    };
}

void add_bc_temp_selection_box(struct boundary_condition_view *bc_vws, GtkWidget *vbox)
{
    GtkTreeModel *model_default =  gtk_tree_view_get_model(bc_vws->bc_tree_view);
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *combobox_add = gtk_combo_box_new_with_model(model_default);
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	
	add_chara2_real_list_box_w_addbottun(bc_vws->bc_tree_view,
				button_add, button_delete, vbox);
	
    /* Add callbacks */
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(cb_add_thermal_bc), (gpointer) bc_vws);
    g_signal_connect(G_OBJECT(combobox_add), "changed", 
                     G_CALLBACK(cb_add_thermal_bc_by_list), (gpointer) bc_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(cb_delete_thermal_bc_by_list), (gpointer) bc_vws);
};

