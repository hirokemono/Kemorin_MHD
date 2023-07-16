/*
//  control_panel_4_MHD_evo_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef CONTROL_PANEL_4_MHD_EVO_GTK_H_
#define CONTROL_PANEL_4_MHD_EVO_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_chara_IO.h"
#include "control_combobox_GTK.h"


struct chara_cbox_table_view{
    int index_bc;
    GtkWidget *clist_tree_view;
    GtkWidget *items_tree_view;
    
    struct chara_clist *ctl_clist_gtk;
};


/* prototypes */

struct chara_cbox_table_view * init_chara_cbox_table_view(struct chara_clist *ctl_clist,
                                                          struct chara_clist *item_clist);


#endif /* CONTROL_PANEL_4_MHD_EVO_GTK_H_ */
