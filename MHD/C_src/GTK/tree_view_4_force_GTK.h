/*
//  tree_view_4_force_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef tree_view_4_force_GTK_h_
#define tree_view_4_force_GTK_h_

#include <stdlib.h>
#include <gtk/gtk.h>

#include "t_ctl_data_SGS_MHD_model_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_chara_real_GTK.h"

struct dimless_views{
    int index_dless;
    GtkWidget *dimless_tree_view;
    
    GtkWidget *default_dless_view;
    
    struct dimless_ctl_c *dless_ctl_gtk;
};

/* prototypes */

void init_dimless_views_GTK(struct mhd_model_control_c *model_ctl, struct dimless_views *dless_vws);
void dealloc_dimless_views_GTK(struct dimless_views *dless_vws);

void init_dimless_tree_view(struct dimless_views *dless_vws);
void create_used_dimless_tree_views(struct dimless_views *dless_vws);

#endif /* tree_view_4_force_GTK_h_ */
