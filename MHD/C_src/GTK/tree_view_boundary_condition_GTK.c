/*
//  tree_view_boundary_condition_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/16.
*/

#include "tree_view_boundary_condition_GTK.h"


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

void load_clist_to_chara2_real_array(struct chara2_real_clist *c2r_clst){
    int i;
    for(i=0;i<count_chara2_real_clist(c2r_clst);i++){
        c_store_chara2_real_array(c2r_clst->f_self, i,
                                  chara2_real_clist_at_index(i,c2r_clst)->c1_tbl,
                                  chara2_real_clist_at_index(i,c2r_clst)->c2_tbl,
                                  chara2_real_clist_at_index(i,c2r_clst)->r_data);
    }
    return;
}


static void bc_position_edited_cb(GtkCellRendererText *cell, gchar *path_str,
            gchar *new_text, gpointer user_data)
{
    struct boundary_condition_view *bc_vws = (struct boundary_condition_view *) user_data;
    
    c2r_tree_2nd_text_edited(path_str, new_text, GTK_TREE_VIEW(bc_vws->bc_tree_view),
                             bc_vws->bc_clist_gtk);
    write_chara2_real_clist(stdout, 0, "bounday location changed", bc_vws->bc_clist_gtk);
    load_clist_to_chara2_real_array(bc_vws->bc_clist_gtk);
}
static void bc_type_edited_cb(GtkCellRendererText *cell, gchar *path_str,
            gchar *new_text, gpointer user_data)
{
    struct boundary_condition_view *bc_vws = (struct boundary_condition_view *) user_data;
    
    c2r_tree_1st_text_edited(path_str, new_text, GTK_TREE_VIEW(bc_vws->bc_tree_view),
                             bc_vws->bc_clist_gtk);
    write_chara2_real_clist(stdout, 0, "BC type changed", bc_vws->bc_clist_gtk);
    load_clist_to_chara2_real_array(bc_vws->bc_clist_gtk);
}
static void bc_value_edited_cb(GtkCellRendererText *cell, gchar *path_str,
            gchar *new_text, gpointer user_data)
{
    struct boundary_condition_view *bc_vws = (struct boundary_condition_view *) user_data;
    
    c2r_tree_value_edited(path_str, new_text, GTK_TREE_VIEW(bc_vws->bc_tree_view),
                          bc_vws->bc_clist_gtk);
    write_chara2_real_clist(stdout, 0, "buoyancy changed", bc_vws->bc_clist_gtk);
    load_clist_to_chara2_real_array(bc_vws->bc_clist_gtk);
}

static void cb_delete_boundary_by_list(GtkButton *button, gpointer user_data)
{
    struct boundary_condition_view *bc_vws = (struct boundary_condition_view *) user_data;
    if(count_chara2_real_clist(bc_vws->bc_clist_gtk) < 3) return;
    
    delete_c2r_list_items_GTK(GTK_TREE_VIEW(bc_vws->bc_tree_view), bc_vws->bc_clist_gtk);
    write_chara2_real_clist(stdout, 0, "buoyancy coeffient deleted", bc_vws->bc_clist_gtk);
    reflesh_f_ctl_c2r_array(count_chara2_real_clist(bc_vws->bc_clist_gtk), bc_vws->bc_clist_gtk);
    load_clist_to_chara2_real_array(bc_vws->bc_clist_gtk);
}


static void cb_add_boundary(GtkButton *button, gpointer user_data)
{
    struct boundary_condition_view *bc_vws = (struct boundary_condition_view *) user_data;
    if(count_chara2_real_clist(bc_vws->bc_clist_gtk) > 1) return;
    
    
    bc_vws->index_bc = add_c2r_list_by_bottun_GTK(bc_vws->index_bc,
                                                  GTK_TREE_VIEW(bc_vws->bc_tree_view),
                                                  bc_vws->bc_clist_gtk);
    write_chara2_real_clist(stdout, 0, "buoyancy coeffient added", bc_vws->bc_clist_gtk);
    reflesh_f_ctl_c2r_array(count_chara2_real_clist(bc_vws->bc_clist_gtk), bc_vws->bc_clist_gtk);
    load_clist_to_chara2_real_array(bc_vws->bc_clist_gtk);
    return;
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
                     G_CALLBACK(bc_value_edited_cb), (gpointer) bc_vws);

    bc_vws->index_bc = append_c2r_list_from_ctl(bc_vws->index_bc, &bc_vws->bc_clist_gtk->c2r_item_head,
                                                GTK_TREE_VIEW(bc_vws->bc_tree_view));
}

static void add_boundary_selection_box(struct boundary_condition_view *bc_vws, GtkWidget *vbox)
{
    GtkWidget *button_add = gtk_button_new_with_label("Add");
    GtkWidget *button_delete = gtk_button_new_with_label("Remove");
    
    add_chara2_real_list_box_w_addbottun(GTK_TREE_VIEW(bc_vws->bc_tree_view),
                                            button_add, button_delete, vbox);
    /* Add callbacks */
    
    g_signal_connect(G_OBJECT(button_add), "clicked",
                     G_CALLBACK(cb_add_boundary), (gpointer) bc_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked",
                     G_CALLBACK(cb_delete_boundary_by_list), (gpointer) bc_vws);
};

GtkWidget * boundary_condition_expander(struct chara2_real_clist *f_bc_ctl, 
                                        struct chara_clist *bc_types, 
                                        struct boundary_condition_view *bc_vws,
                                        GtkWidget *window){
    bc_vws = init_boudary_condition_views_GTK(f_bc_ctl, bc_types);
    init_boundary_condition_tree_view(bc_vws);

    GtkWidget *vbox_m3t = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    add_boundary_selection_box(bc_vws, vbox_m3t);
    GtkWidget *expand_bc = wrap_into_scroll_expansion_gtk(duplicate_underscore(f_bc_ctl->clist_name),
                                                        320, 160, window, vbox_m3t);
    return expand_bc;
}

