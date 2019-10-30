/*
//  tree_view_4_each_term_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/16.
*/

#ifndef tree_view_4_each_term_GTK_h_
#define tree_view_4_each_term_GTK_h_

#include "calypso_GTK.h"
#include "t_ctl_data_SGS_MHD_model_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_4_force_GTK.h"


struct momentum_coefs_view{
    int index_coefs;
    GtkTreeView *coefs_tree_view;
    GtkTreeView *dimless_tree_view;
    
    struct momentum_equation_ctl_c *mom_ctl_gtk;
};

struct coefs_view{
    struct dimless_views *dless_vws;
    struct momentum_coefs_view *mom_vws;
};

/* prototypes */

void init_momentum_views_GTK(struct chara_real_clist *cr_clist,
                                    struct dimless_views *dless_vws, 
                                    struct momentum_coefs_view *mom_vws);

void init_coefs_views_GTK(struct mhd_model_control_c *model_ctl, struct coefs_view *coef_vws);

void init_momentum_tree_view(struct momentum_coefs_view *mom_vws);
void add_thermal_buo_selection_box(struct momentum_coefs_view *mom_vws, GtkWidget *vbox);

#endif /* tree_view_4_each_term_GTK_h_ */
