/*
//  control_panel_4_coefs_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/16.
*/

#ifndef control_panel_4_coefs_GTK_h_
#define control_panel_4_coefs_GTK_h_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_ctl_data_4_fields_c.h"
#include "tree_views_4_fixed_lists_GTK.h"

struct momentum_coefs_view{
    int index_coefs;
    GtkTreeView *coefs_tree_view;
    GtkTreeView *dimless_tree_view;
    
    struct momentum_equation_ctl_c *mom_ctl_gtk;
};

/* prototypes */

void add_coefs_selection_box(struct momentum_coefs_view *coefs_vw, GtkWidget *vbox);


#endif /* control_panel_4_coefs_GTK_h_ */
