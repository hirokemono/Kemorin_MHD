/*
//  control_panel_real_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/17.
*/

#ifndef CONTROL_PANEL_REAL_GTK_H_
#define CONTROL_PANEL_REAL_GTK_H_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_real_IO.h"
#include "t_ctl_array_single_items_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_real_GTK.h"

struct r_clist_view{
    int index_bc;
    GtkWidget *real_array_tree_view;
    
    struct real_clist *r_clist_gtk;
};

/* prototypes */

GtkWidget * real_array_vbox_w_addbottun(struct real_clist *r_clist_gtk, 
										GtkWidget *real_array_tree_view);

#endif /* CONTROL_PANEL_REAL_GTK_H_ */
