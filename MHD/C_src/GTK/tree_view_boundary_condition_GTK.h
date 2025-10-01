/*
//  tree_view_boundary_condition_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/16.
*/

#ifndef tree_view_boundary_condition_GTK_h_
#define tree_view_boundary_condition_GTK_h_

#include "calypso_GTK.h"
#include "t_control_chara2_real_IO.h"
#include "t_ctl_array_chara2_real_items_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_chara2_real_GTK.h"
#include "tree_view_chara_GTK.h"
#include "control_combobox_GTK.h"
#include "kemoview_gtk_routines.h"


struct boundary_condition_view{
    int index_bc;
    GtkWidget *bc_tree_view;
    GtkWidget *bc_type_tree_view;
    
    struct chara2_real_clist *bc_clist_gtk;
};

/* prototypes */

void load_clist_to_chara2_real_array(struct chara2_real_clist *c2r_clst);


struct boundary_condition_view * init_boudary_condition_views_GTK(struct chara2_real_clist *bc_ctl,
                                                                  struct chara_clist *bc_types);
void init_bc_temp_tree_view(struct boundary_condition_view *bc_vws);

GtkWidget * boundary_condition_expander(struct chara2_real_clist *f_bc_ctl, 
                                        struct chara_clist *bc_types, 
                                        struct boundary_condition_view *bc_vws,
                                        GtkWidget *window);

#endif /* tree_view_boundary_condition_GTK_h_ */
