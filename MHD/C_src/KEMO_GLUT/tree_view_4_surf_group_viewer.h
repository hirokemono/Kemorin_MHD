/*
//  tree_view_4_surf_group_viewer.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#ifndef TREE_VIEW_4_SURF_GROUP_VIEWER_
#define TREE_VIEW_4_SURF_GROUP_VIEWER_

#include <gtk/gtk.h>

#include "m_kemoview_mesh_menu.h"
#include "t_control_chara_int3_IO.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_chara_int3_GTK.h"
#include "tree_view_4_viewer_mesh.h"
#include "kemoview_gtk_routines.h"
#include "kemoview_gtk_colorsel.h"
#include "create_tree_view_columns_GTK.h"
#include "quicksort_c.h"

#ifdef GLFW3
	#include "view_modifier_glfw.h"
#else
	#include "view_modifier_gtk.h"
#endif


/* prototypes */

void add_surf_group_draw_box(struct mesh_menu_val *mesh_m, struct ci3_clist_view *surf_grp_vws, 
							 GtkWidget *window_mesh, GtkWidget *vbox);

#endif /* tree_view_4_surf_group_viewer */
