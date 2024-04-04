/*
//  control_panel_cbox_cbox_real_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_PANEL_CBOX_CBOX_REAL_GTK_H_
#define CONTROL_PANEL_CBOX_CBOX_REAL_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "kemoview_gtk_routines.h"
#include "t_control_chara_IO.h"
#include "t_control_chara2_real_IO.h"
#include "t_ctl_array_chara_real_items_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_boundary_condition_GTK.h"
#include "control_combobox_GTK.h"
#include "control_panel_field_cbox_GTK.h"

struct cbox_cbox_table_view{
    GtkWidget *clist_tree_view;
    GtkWidget *item1_tree_view;
    GtkWidget *item2_tree_view;
};


/* prototypes */

struct cbox_cbox_table_view * init_cbox_cbox_table_view(struct chara2_int_clist *item1_clist,
                                                        struct chara2_int_clist *item2_clist);

GtkWidget * c2r_list_combobox_expander(struct chara2_real_clist *ctl_clist,
                                       struct chara2_int_clist *item1_clist,
                                       struct chara2_int_clist *item2_clist,
                                       struct cbox_cbox_table_view *cbox2_tbl_vws,
                                       GtkWidget *window);

#endif /* CONTROL_PANEL_CBOX_CBOX_REAL_GTK_H_ */
