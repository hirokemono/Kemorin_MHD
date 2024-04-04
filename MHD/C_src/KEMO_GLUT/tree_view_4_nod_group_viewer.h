/*
//  tree_view_4_nod_group_viewer.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#ifndef TREE_VIEW_4_NOD_GROUP_VIEWER_
#define TREE_VIEW_4_NOD_GROUP_VIEWER_

#include "calypso_GTK.h"
#include "kemoviewer.h"
#include "kemoview_gtk_routines.h"
#include "kemoview_gtk_colorsel.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "t_control_chara_int_IO.h"
#include "tree_view_chara_int_GTK.h"
#include "tree_view_4_viewer_mesh.h"
#include "quicksort_c.h"

#ifdef GLFW3
	#include "view_modifier_glfw.h"
#else
	#include "view_modifier_gtk.h"
#endif

/* prototypes */
void set_nod_group_draw_box(struct kemoviewer_type *kemo_sgl,
                            struct nod_grp_gtk_menu *node_group_gmenu);
void init_nod_group_draw_expander(struct kemoviewer_type *kemo_sgl,
                                  GtkWidget *window, 
                                  struct nod_grp_gtk_menu *node_group_gmenu);
GtkWidget * pack_nod_group_draw_expander(GtkWidget *window, struct nod_grp_gtk_menu *node_group_gmenu);

#endif /* tree_view_4_nod_group_viewer */
