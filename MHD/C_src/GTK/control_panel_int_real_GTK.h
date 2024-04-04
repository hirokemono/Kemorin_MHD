/*
//  control_panel_int_real_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_PANEL_INT_REAL_GTK_H_
#define CONTROL_PANEL_INT_REAL_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_int_real_IO.h"
#include "t_ctl_array_int_real_items_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_int_real_GTK.h"

/* prototypes */

GtkWidget *  add_ir_list_box_w_addbottun(struct int_real_clist *ir_clist_gtk,
										 GtkWidget *ir_tree_view);

#endif /* CONTROL_PANEL_INT_REAL_GTK_H_ */
