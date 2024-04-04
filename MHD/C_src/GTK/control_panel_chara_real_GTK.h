/*
//  control_panel_chara_real_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_PANEL_CHARA_REAL_GTK_H_
#define CONTROL_PANEL_CHARA_REAL_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_chara_real_IO.h"
#include "t_ctl_array_chara_real_items_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_chara_real_GTK.h"

/* prototypes */

GtkWidget * add_cr_list_box_w_addbottun(struct chara_real_clist *cr_clist_gtk,
                                        GtkWidget *cr_tree_view);

#endif /* CONTROL_PANEL_CHARA_REAL_GTK_H_ */
