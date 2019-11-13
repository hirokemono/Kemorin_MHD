/*
//  tree_view_4_ele_group_viewer.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#ifndef TREE_VIEW_4_ELE_GROUP_VIEWER_
#define TREE_VIEW_4_ELE_GROUP_VIEWER_

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

struct gtk_ele_grp_menu{
	GtkWidget *combobox_patch_color;
	GtkWidget *combobox_grid_color;
	GtkWidget *combobox_node_color;
	GtkWidget *button_patch_color;
	GtkWidget *button_grid_color;
	GtkWidget *button_node_color;
	GtkWidget *spin_opacity;
	
	GdkRGBA gcolor;
};


/* prototypes */

void set_ele_group_draw_box(struct gtk_ele_grp_menu *gtk_ele_group);
void add_ele_group_draw_box(struct ci3_clist_view *ele_grp_vws, GtkWidget *window_mesh, 
							struct gtk_ele_grp_menu *gtk_ele_group, GtkWidget *vbox);

#endif /* tree_view_4_ele_group_viewer */
