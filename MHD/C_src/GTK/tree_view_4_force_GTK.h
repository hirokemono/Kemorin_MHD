/*
//  tree_view_4_force_GTK.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef tree_view_4_force_GTK_h_
#define tree_view_4_force_GTK_h_

#include <stdlib.h>

#include "calypso_GTK.h"
#include "t_control_chara_real_IO.h"
#include "t_ctl_array_chara_real_items_c.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_chara_real_GTK.h"

struct dimless_views{
    GtkWidget *dimless_tree_view;
    GtkWidget *default_dless_view;
};

/* prototypes */

void update_f_ctl_cr_array_by_cr_list(struct chara_real_clist *cr_clist);
void reflesh_f_ctl_cr_array_by_cr_list(struct chara_real_clist *cr_clist);


void init_dimless_tree_view(struct chara_real_clist *cr_clist, struct dimless_views *dless_vws);
void create_used_dimless_tree_views(struct dimless_views *dless_vws);

#endif /* tree_view_4_force_GTK_h_ */
