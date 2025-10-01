/*
//  control_panel_int_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_PANEL_INT_GTK_H_
#define CONTROL_PANEL_INT_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_int_IO.h"
#include "t_ctl_array_single_items_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_int_GTK.h"

/* prototypes */

GtkWidget *  add_int_list_box_w_addbottun(struct int_clist *i_clist_gtk, GtkWidget *int_tree_view);

#endif /* CONTROL_PANEL_INT_GTK_H_ */
