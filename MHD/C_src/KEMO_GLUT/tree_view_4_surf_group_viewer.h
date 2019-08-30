/*
//  tree_view_4_surf_group_viewer.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#ifndef TREE_VIEW_4_SURF_GROUP_VIEWER_
#define TREE_VIEW_4_SURF_GROUP_VIEWER_

#include <gtk/gtk.h>

#include "kemoviewer.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "t_control_chara_int3_IO.h"
#include "tree_view_chara_int3_GTK.h"
#include "tree_view_4_viewer_mesh.h"
#include "create_tree_view_columns_GTK.h"
#include "quicksort_c.h"

/* prototypes */

void create_surface_group_columns(struct ci3_clist_view *surf_grp_vws);
void create_surface_group_view(struct ci3_clist_view *surf_grp_vws);

#endif /* tree_view_4_surf_group_viewer */
