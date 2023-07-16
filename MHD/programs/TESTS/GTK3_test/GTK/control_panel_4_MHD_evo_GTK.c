/*
//  control_panel_4_MHD_evo_GTK.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#include "control_panel_4_MHD_evo_GTK.h"

extern int lengthchara_f(void);

extern void * c_chara_item_clength(void *f_ctl, int *length);

struct chara_cbox_table_view * init_chara_cbox_table_view(struct chara_clist *ctl_clist,
                                                          struct chara_clist *item_clist){
	struct chara_cbox_table_view *chara_tbl_vws  
			= (struct chara_cbox_table_view *) malloc(sizeof(struct chara_cbox_table_view));
	if(chara_tbl_vws == NULL){
		printf("malloc error for chara_cbox_table_view\n");
		exit(0);
	};
	
    chara_tbl_vws->ctl_clist_gtk = ctl_clist;
    chara_tbl_vws->clist_tree_view = gtk_tree_view_new();
    chara_tbl_vws->items_tree_view = create_fixed_label_tree(item_clist);
    return chara_tbl_vws;
}
