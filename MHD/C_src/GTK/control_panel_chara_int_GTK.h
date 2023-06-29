/*
//  control_panel_chara_int_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_PANEL_CHARA_INT_GTK_H_
#define CONTROL_PANEL_CHARA_INT_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_chara_int_IO.h"
#include "t_ctl_array_chara_int_items_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_chara_int_GTK.h"

/* prototypes */

GtkWidget * add_ci_list_box_w_addbottun(struct chara_int_clist *ci_clist_gtk,
                                        GtkWidget *ci_tree_view);

#endif /* CONTROL_PANEL_CHARA_INT_GTK_H_ */
