/*
//  tree_view_kemoview_colormap.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#ifndef TREE_VIEW_KEMOVIEW_COLORMAP_
#define TREE_VIEW_KEMOVIEW_COLORMAP_

#include "calypso_GTK.h"
#include "kemoviewer.h"
#include "tree_view_4_colormap.h"

#ifdef GLFW3
	#include "view_modifier_glfw.h"
#else
	#include "view_modifier_gtk.h"
#endif


/* prototypes */

void update_kemoview_cmap_list_box(struct colormap_view *color_vws);
GtkWidget * init_kemoview_colormap_list_vbox(struct colormap_view *color_vws);
	
#endif /* TREE_VIEW_KEMOVIEW_COLORMAP_ */
