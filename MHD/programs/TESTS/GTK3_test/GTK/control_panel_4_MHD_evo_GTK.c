/*
//  control_panel_4_MHD_evo_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#include "control_panel_4_MHD_evo_GTK.h"

extern int lengthchara_f(void);

extern void * c_chara_item_clength(void *f_ctl, int *length);

extern void * c_MHD_evolution_ctl_block_name(void *f_evo_ctl);
extern void * c_MHD_evolution_ctl_iflag(void *f_evo_ctl);
extern void * c_MHD_t_evo_field_ctl(void *f_evo_ctl);


struct boundary_condition_view * init_boudary_condition_views_GTK(struct chara2_real_clist *bc_ctl,
                                                                  struct chara_clist *bc_types){
	struct boundary_condition_view *bc_vws  
			= (struct boundary_condition_view *) malloc(sizeof(struct boundary_condition_view));
	if(bc_vws == NULL){
		printf("malloc error for boundary_condition_view\n");
		exit(0);
	};
	
    bc_vws->bc_clist_gtk = bc_ctl;
    bc_vws->bc_tree_view =      gtk_tree_view_new();
    bc_vws->bc_type_tree_view = create_fixed_label_tree(bc_types);
    return bc_vws;
}

void init_boundary_condition_tree_view(struct boundary_condition_view *bc_vws){
    GtkCellRenderer *renderer_cbox = gtk_cell_renderer_combo_new();
    GtkCellRenderer *renderer_text = gtk_cell_renderer_text_new();
    GtkCellRenderer *renderer_spin = gtk_cell_renderer_spin_new();
    
    GtkTreeModel *cbox_model
            = gtk_tree_view_get_model(GTK_TREE_VIEW(bc_vws->bc_type_tree_view));

    
    create_cbox_text_real_tree_view(GTK_LIST_STORE(cbox_model), GTK_TREE_VIEW(bc_vws->bc_tree_view),
                                    renderer_cbox, renderer_text, renderer_spin);
    g_signal_connect(G_OBJECT(renderer_cbox), "edited",
                     G_CALLBACK(bc_type_edited_cb), (gpointer) bc_vws);
    g_signal_connect(G_OBJECT(renderer_text), "edited",
                     G_CALLBACK(bc_position_edited_cb), (gpointer) bc_vws);
    g_signal_connect(G_OBJECT(renderer_spin), "edited",
                     G_CALLBACK(thermal_bc_value_edited_cb), (gpointer) bc_vws);

    bc_vws->index_bc = append_c2r_list_from_ctl(bc_vws->index_bc, &bc_vws->bc_clist_gtk->c2r_item_head,
                                                GTK_TREE_VIEW(bc_vws->bc_tree_view));
}

static void add_bc_temp_selection_box(struct boundary_condition_view *bc_vws, GtkWidget *vbox)
{
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
    
    add_chara2_real_list_box_w_addbottun(GTK_TREE_VIEW(bc_vws->bc_tree_view),
                                            button_add, button_delete, vbox);
    /* Add callbacks */
    
    g_signal_connect(G_OBJECT(button_add), "clicked",
                     G_CALLBACK(cb_add_thermal_bc), (gpointer) bc_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked",
                     G_CALLBACK(cb_delete_thermal_bc_by_list), (gpointer) bc_vws);
};

GtkWidget * boundary_condition_expander(struct chara2_real_clist *f_bc_ctl, 
                                        struct chara_clist *bc_types, 
                                        struct boundary_condition_view *bc_vws,
                                        GtkWidget *window){
    bc_vws = init_boudary_condition_views_GTK(f_bc_ctl, bc_types);
    init_boundary_condition_tree_view(bc_vws);

    GtkWidget *vbox_m3t = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    add_bc_temp_selection_box(bc_vws, vbox_m3t);
    GtkWidget *expand_bc = wrap_into_expanded_frame_gtk(duplicate_underscore(f_bc_ctl->clist_name),
                                                        320, 160, window, vbox_m3t);
    return expand_bc;
}
