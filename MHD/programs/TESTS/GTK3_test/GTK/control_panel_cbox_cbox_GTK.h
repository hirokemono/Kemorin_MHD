/*
//  control_panel_cbox_cbox_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_PANEL_CBOX_CBOX_GTK_H_
#define CONTROL_PANEL_CBOX_CBOX_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "kemoview_gtk_routines.h"
#include "t_control_chara_IO.h"
#include "t_control_chara2_IO.h"
#include "t_ctl_array_single_items_c.h"
#include "t_ctl_array_chara2_items_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "control_combobox_GTK.h"

struct chara2_cbox_table_view{
    int index_bc;
    GtkWidget *clist_tree_view;
    GtkWidget *item1_tree_view;
    GtkWidget *item2_tree_view;
};

/* prototypes */

GtkWidget * c2_list_cbox_cbox_expander(struct chara2_clist *ctl_clist,
                                       struct chara_clist *item1_clist, struct chara_clist *item2_clist,
                                       struct chara2_cbox_table_view *chara2_tbl_vws,
                                       GtkWidget *window);

#endif /* CONTROL_PANEL_CBOX_CBOX_GTK_H_ */
