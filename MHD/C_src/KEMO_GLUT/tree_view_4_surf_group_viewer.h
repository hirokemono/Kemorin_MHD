/*
//  tree_view_4_surf_group_viewer.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#ifndef TREE_VIEW_4_SURF_GROUP_VIEWER_
#define TREE_VIEW_4_SURF_GROUP_VIEWER_

#include "calypso_GTK.h"
#include "kemoviewer.h"
#include "t_control_chara_int3_IO.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_chara_int3_GTK.h"
#include "tree_view_4_viewer_mesh.h"
#include "kemoview_gtk_routines.h"
#include "kemoview_gtk_colorsel.h"
#include "quicksort_c.h"

#ifdef GLFW3
	#include "view_modifier_glfw.h"
#else
	#include "view_modifier_gtk.h"
#endif

/* prototypes */
GtkWidget * init_surf_group_draw_expander(GtkWidget *window, struct group_gtk_menu *surf_group_gmenu);

#endif /* tree_view_4_surf_group_viewer */
