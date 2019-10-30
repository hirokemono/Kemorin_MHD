/*
//  tree_view_4_temp_BC_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/16.
*/

#ifndef tree_view_4_temp_BC_GTK_h_
#define tree_view_4_temp_BC_GTK_h_

#include "calypso_GTK.h"
#include "t_control_chara2_real_IO.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_4_temp_BC_GTK.h"
#include "tree_view_chara2_real_GTK.h"


struct boundary_condition_view{
    int index_bc;
    GtkTreeView *bc_tree_view;
    GtkTreeView *bc_type_tree_view;
    
    struct chara2_real_clist *bc_T_gtk;
};

/* prototypes */

void init_temp_bc_views_GTK(struct chara2_real_clist *bc_T_ctl,
                                    struct boundary_condition_view *bc_vws);

void init_bc_temp_tree_view(struct boundary_condition_view *bc_vws);

void add_bc_temp_selection_box(struct boundary_condition_view *bc_vws, GtkWidget *vbox);


#endif /* tree_view_4_temp_BC_GTK_h_ */
