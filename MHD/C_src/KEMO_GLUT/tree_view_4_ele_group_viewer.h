/*
//  tree_view_4_ele_group_viewer.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#ifndef TREE_VIEW_4_ELE_GROUP_VIEWER_
#define TREE_VIEW_4_ELE_GROUP_VIEWER_

#include <gtk/gtk.h>

#include "kemoviewer.h"
#include "kemoview_gtk_routines.h"
#include "kemoview_gtk_colorsel.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "t_control_chara_int3_IO.h"
#include "tree_view_chara_int3_GTK.h"
#include "tree_view_4_viewer_mesh.h"
#include "create_tree_view_columns_GTK.h"
#include "quicksort_c.h"
#include "view_modifier_glfw.h"

/* prototypes */

void add_ele_group_draw_box(struct ci3_clist_view *ele_grp_vws,
							GtkWidget *window_mesh, GtkWidget *vbox);

#endif /* tree_view_4_ele_group_viewer */
