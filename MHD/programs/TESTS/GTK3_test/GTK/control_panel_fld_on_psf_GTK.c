/*
//  control_panel_fld_on_psf_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#include "control_panel_fld_on_psf_GTK.h"


static struct chara2_cbox_table_view * init_chara2_cbox_table_view(struct chara_int2_clist *f_field_ctl,
                                                           struct chara_clist *item2_clist){
    struct chara2_cbox_table_view *chara2_tbl_vws
            = (struct chara2_cbox_table_view *) malloc(sizeof(struct chara2_cbox_table_view));
    if(chara2_tbl_vws == NULL){
        printf("malloc error for chara2_cbox_table_view\n");
        exit(0);
    };
    
    chara2_tbl_vws->clist_tree_view = gtk_tree_view_new();
    chara2_tbl_vws->item1_tree_view = create_field_label_tree(f_field_ctl);
    chara2_tbl_vws->item2_tree_view = create_fixed_label_tree(item2_clist);
    return chara2_tbl_vws;
}


GtkWidget * c2_list_fld_on_psf_expander(struct chara2_clist *ctl_clist,
                                        struct chara_int2_clist *f_field_ctl, 
                                        struct chara_clist *item2_clist,
                                        struct chara2_cbox_table_view *chara2_tbl_vws,
                                        GtkWidget *window){
    chara2_tbl_vws = init_chara2_cbox_table_view(f_field_ctl, item2_clist);
    init_c2_combobox_tree_view(ctl_clist, chara2_tbl_vws);

    GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    add_c2_list_selection_box(ctl_clist, chara2_tbl_vws, vbox);
    GtkWidget *expand_bc = wrap_into_expanded_frame_gtk(duplicate_underscore(ctl_clist->clist_name),
                                                        window, vbox);
    return expand_bc;
}

