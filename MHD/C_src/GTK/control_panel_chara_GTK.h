/*
//  control_panel_chara_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_PANEL_CHARA_GTK_H_
#define CONTROL_PANEL_CHARA_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_chara_IO.h"
#include "t_ctl_array_single_items_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_chara_GTK.h"

/* prototypes */

GtkWidget * add_c_list_box_w_addbottun(struct chara_clist *c_clist_gtk,
									   GtkWidget *c_tree_view);

#endif /* CONTROL_PANEL_CHARA_GTK_H_ */
