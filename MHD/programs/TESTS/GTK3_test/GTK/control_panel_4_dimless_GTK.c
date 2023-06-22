/*
//  control_panel_4_dimless_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#include "control_panel_4_dimless_GTK.h"

extern void * c_chara_item_clength(void *f_ctl, int *length);
extern void * c_MHD_dimless_block_name(void *f_dimless_ctl);
extern void * c_MHD_dimless_iflag(void *f_dimless_ctl);
extern void * c_MHD_dimless_array(void *f_dimless_ctl);

struct f_MHD_dimless_control * init_f_MHD_dimless_ctl(void *(*c_load_self)(void *f_parent),
													 void *f_parent)
{
	struct f_MHD_dimless_control *f_dimless_ctl
			= (struct f_MHD_dimless_control *) malloc(sizeof(struct f_MHD_dimless_control));
	if(f_dimless_ctl == NULL){
		printf("malloc error for f_dimless_ctl\n");
		exit(0);
	};
	
	f_dimless_ctl->f_self =  c_load_self(f_parent);
	
	f_dimless_ctl->f_block_name =   (char *) c_MHD_dimless_block_name(f_dimless_ctl->f_self);
	f_dimless_ctl->f_iflag =        (int *)  c_MHD_dimless_iflag(f_dimless_ctl->f_self);
	
	c_chara_item_clength(f_dimless_ctl->f_block_name, f_dimless_ctl->f_namelength);
	f_dimless_ctl->c_block_name = alloc_string((long) f_dimless_ctl->f_namelength[0]);
	strngcopy_w_length(f_dimless_ctl->c_block_name, f_dimless_ctl->f_namelength[0], 
					   f_dimless_ctl->f_block_name);
	
	f_dimless_ctl->f_dimess_names = init_f_ctl_cr_array(c_MHD_dimless_array, f_dimless_ctl->f_self);
//	f_dimless_ctl->f_dimless_vws =  init_cr_array_views(f_dimless_ctl->f_dimess_names);
	
    f_dimless_ctl->f_dimless_vws = (struct dimless_views *) malloc(sizeof(struct dimless_views));
	int i;
    f_dimless_ctl->f_dimless_vws->cr_clist = init_chara_real_clist();
    for(i=0;i<f_dimless_ctl->f_dimess_names->f_num[0];i++){
		append_chara_real_clist(f_dimless_ctl->f_dimess_names->c_charavalue[i], 
								f_dimless_ctl->f_dimess_names->f_rctls[i],
                                f_dimless_ctl->f_dimless_vws->cr_clist);
    }
	
	return f_dimless_ctl;
};



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


GtkWidget * add_dimless_selection_box(struct f_MHD_dimless_control *f_dless_ctl, GtkWidget *window)
{
	GtkWidget *vbox_dimless = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    GtkTreeModel *model_default =  gtk_tree_view_get_model(GTK_TREE_VIEW(f_dless_ctl->f_dimless_vws->default_dless_view));
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *combobox_add = gtk_combo_box_new_with_model(model_default);
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
	
	add_chara_real_list_box_w_combobox(GTK_TREE_VIEW(f_dless_ctl->f_dimless_vws->dimless_tree_view),
				button_add, combobox_add, button_delete, vbox_dimless);
	
    /* Add callbacks */
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(cb_add_dimless_new), (gpointer) f_dless_ctl->f_dimless_vws);
    g_signal_connect(G_OBJECT(combobox_add), "changed", 
                     G_CALLBACK(cb_add_dimless_name), (gpointer) f_dless_ctl->f_dimless_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
					 G_CALLBACK(cb_deleta_dimless_lists), (gpointer) f_dless_ctl->f_dimless_vws);
	
	GtkWidget *expand_MHD_dimless = draw_control_block(f_dless_ctl->c_block_name, f_dless_ctl->f_iflag,
													   400, 240, window, vbox_dimless);
	return expand_MHD_dimless;
};
