/*
//  control_panel_4_forces_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#include "control_panel_4_forces_GTK.h"

extern int lengthchara_f(void);

extern void * c_chara_item_clength(void *f_ctl, int *length);
extern void * c_MHD_dimless_block_name(void *f_dimless_ctl);
extern void * c_MHD_dimless_iflag(void *f_dimless_ctl);
extern void * c_MHD_dimless_array(void *f_dimless_ctl);


static void cb_delete_dimless_lists(GtkButton *button, gpointer user_data)
{
    struct f_MHD_dimless_control *f_dless_ctl = (struct f_MHD_dimless_control *) user_data;
	struct dimless_views *f_dimless_vws
			= (struct dimless_views *) g_object_get_data(G_OBJECT(button), "f_dimless_vws");
    
    delete_cr_list_items_GTK(GTK_TREE_VIEW(f_dimless_vws->dimless_tree_view), f_dless_ctl->f_dimess_names);
	reflesh_f_ctl_cr_array_by_cr_list(f_dless_ctl->f_dimess_names);
}

static void cb_add_dimless_new(GtkButton *button, gpointer user_data)
{
    struct f_MHD_dimless_control *f_dless_ctl = (struct f_MHD_dimless_control *) user_data;
	struct dimless_views *f_dimless_vws
			= (struct dimless_views *) g_object_get_data(G_OBJECT(button), "f_dimless_vws");
	
	f_dless_ctl->f_dimess_names->index_bc = add_cr_list_items_GTK(GTK_TREE_VIEW(f_dimless_vws->dimless_tree_view),
                                                   f_dless_ctl->f_dimess_names);
	reflesh_f_ctl_cr_array_by_cr_list(f_dless_ctl->f_dimess_names);
    return;
}

static void cb_add_dimless_name(GtkComboBox *combobox_add, gpointer user_data)
{
    struct f_MHD_dimless_control *f_dless_ctl = (struct f_MHD_dimless_control *) user_data;
	struct dimless_views *f_dimless_vws
			= (struct dimless_views *) g_object_get_data(G_OBJECT(combobox_add), "f_dimless_vws");
	
	GtkTreeModel *model_comp = gtk_combo_box_get_model(combobox_add);  
	
    gint idx = gtk_combo_box_get_active(combobox_add);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
	f_dless_ctl->f_dimess_names->index_bc = add_cr_list_from_combobox_GTK(f_dless_ctl->f_dimess_names->index_bc,
				path, model_comp, GTK_TREE_VIEW(f_dimless_vws->dimless_tree_view), f_dless_ctl->f_dimess_names);
	reflesh_f_ctl_cr_array_by_cr_list(f_dless_ctl->f_dimess_names);
    return;
}


GtkWidget * add_dimless_selection_box(struct f_MHD_dimless_control *f_dless_ctl, 
									  struct dimless_views * f_dimless_vws, GtkWidget *window)
{
	f_dimless_vws = (struct dimless_views *) malloc(sizeof(struct dimless_views));
	if(f_dimless_vws == NULL){
		printf("malloc error for dimless_views\n");
		exit(0);
	};
	
	GtkWidget *vbox_dimless = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	
	f_dimless_vws->dimless_tree_view = gtk_tree_view_new();
    init_dimless_tree_view(f_dless_ctl->f_dimess_names, f_dimless_vws);
    create_used_dimless_tree_views(f_dimless_vws);
	
    GtkTreeModel *model_default =  gtk_tree_view_get_model(GTK_TREE_VIEW(f_dimless_vws->default_dless_view));
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *combobox_add = gtk_combo_box_new_with_model(model_default);
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	
	add_chara_real_list_box_w_combobox(GTK_TREE_VIEW(f_dimless_vws->dimless_tree_view),
				button_add, combobox_add, button_delete, vbox_dimless);
	
	/* Add callbacks */
	g_object_set_data(G_OBJECT(button_add),    "f_dimless_vws", (gpointer) f_dimless_vws);
	g_object_set_data(G_OBJECT(combobox_add),  "f_dimless_vws", (gpointer) f_dimless_vws);
	g_object_set_data(G_OBJECT(button_delete), "f_dimless_vws", (gpointer) f_dimless_vws);
	
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(cb_add_dimless_new), (gpointer) f_dless_ctl);
    g_signal_connect(G_OBJECT(combobox_add), "changed", 
                     G_CALLBACK(cb_add_dimless_name), (gpointer) f_dless_ctl);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
					 G_CALLBACK(cb_delete_dimless_lists), (gpointer) f_dless_ctl);
	
	GtkWidget *expand_MHD_dimless = draw_control_block(strngcopy_from_f(f_dless_ctl->c_block_name),
													   f_dless_ctl->f_iflag,
													   window, vbox_dimless);
	return expand_MHD_dimless;
};
