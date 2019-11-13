/*
//  tree_view_4_domain_viewer.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#ifndef TREE_VIEW_4_DOMAIN_VIEWER_
#define TREE_VIEW_4_DOMAIN_VIEWER_

#include "calypso_GTK.h"
#include "kemoviewer.h"
#include "kemoview_gtk_routines.h"
#include "kemoview_gtk_colorsel.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "t_control_chara_int3_IO.h"
#include "tree_view_chara_int3_GTK.h"
#include "tree_view_4_viewer_mesh.h"
#include "quicksort_c.h"

#ifdef GLFW3
	#include "view_modifier_glfw.h"
#else
	#include "view_modifier_gtk.h"
#endif


/* prototypes */

void set_domain_draw_box(struct gtk_group_menu *gtk_domain_group);
void add_domain_draw_box(struct ci3_clist_view *domain_vws, GtkWidget *window_mesh, 
						 struct gtk_group_menu *gtk_domain_group, GtkWidget *vbox);

#endif /* tree_view_4_domain_viewer */
