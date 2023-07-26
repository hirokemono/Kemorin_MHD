/*
//  control_panel_fld_on_psf_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_fld_on_psf_GTK.h"


static GtkWidget * create_direction_label_tree(struct chara2_int_clist *label_dir_list)
{
    GtkWidget *ctl_flags_tree_view = gtk_tree_view_new();
    
    /* Construct empty list storage */
    GtkListStore *child_model = gtk_list_store_new(1, G_TYPE_STRING);
    /* Construct model for sorting and set to tree view */
    GtkTreeModel *model = gtk_tree_model_sort_new_with_model(GTK_TREE_MODEL(child_model));
    /* Append items */
    int i;
    GtkTreeIter iter;
    struct chara2_int_ctl_item *citem_tmp;
    for(i=0;i<count_chara2_int_clist(label_dir_list);i++){
        citem_tmp = chara2_int_clist_at_index(i, label_dir_list);
        gtk_list_store_append(GTK_LIST_STORE(child_model), &iter);
        gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                           COLUMN_FIELD_INDEX, citem_tmp->c1_tbl, -1);
    };
    
    gtk_tree_view_set_model(GTK_TREE_VIEW(ctl_flags_tree_view), model);
    return ctl_flags_tree_view;
};

static struct chara2_cbox_table_view * init_chara2_cbox_table_view(struct chara_int2_clist *f_field_ctl,
                                                                   struct chara2_int_clist *label_dir_list){
    struct chara2_cbox_table_view *chara2_tbl_vws
            = (struct chara2_cbox_table_view *) malloc(sizeof(struct chara2_cbox_table_view));
    if(chara2_tbl_vws == NULL){
        printf("malloc error for chara2_cbox_table_view\n");
        exit(0);
    };
    
    chara2_tbl_vws->clist_tree_view = gtk_tree_view_new();
    chara2_tbl_vws->item1_tree_view = create_field_label_tree(f_field_ctl);
    chara2_tbl_vws->item2_tree_view = create_direction_label_tree(label_dir_list);
    return chara2_tbl_vws;
}


GtkWidget * c2_list_fld_on_psf_expander(struct chara2_clist *ctl_clist,
                                        struct chara_int2_clist *f_field_ctl, 
                                        struct chara2_int_clist *label_dir_list,
                                        struct chara2_cbox_table_view *chara2_tbl_vws,
                                        GtkWidget *window){
    chara2_tbl_vws = init_chara2_cbox_table_view(f_field_ctl, label_dir_list);
    init_c2_combobox_tree_view(ctl_clist, chara2_tbl_vws);

    GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    add_c2_list_selection_box(ctl_clist, chara2_tbl_vws, vbox);
    GtkWidget *expand_bc = wrap_into_expanded_frame_gtk(duplicate_underscore(ctl_clist->clist_name),
                                                        window, vbox);
    return expand_bc;
}

