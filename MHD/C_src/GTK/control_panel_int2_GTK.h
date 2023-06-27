/*
//  control_panel_int2_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_PANEL_INT2_GTK_H_
#define CONTROL_PANEL_INT2_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_int2_IO.h"
#include "t_ctl_array_int2_items_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_int2_GTK.h"

struct i2_clist_view{
    int index_bc;
    GtkWidget *i2_tree_view;
    
    struct int2_clist *i2_clist_gtk;
};

/* prototypes */

GtkWidget * add_i2_list_box_w_addbottun(struct i2_clist_view *i2_vws);

#endif /* CONTROL_PANEL_INT2_GTK_H_ */
