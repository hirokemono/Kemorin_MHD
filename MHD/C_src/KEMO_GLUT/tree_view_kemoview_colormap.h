/*
//  tree_view_kemoview_colormap.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#ifndef TREE_VIEW_KEMOVIEW_COLORMAP_
#define TREE_VIEW_KEMOVIEW_COLORMAP_

#include "calypso_GTK.h"
#include "kemoviewer_gl.h"
#include "tree_view_4_colormap.h"

#include "view_modifier_glfw.h"


/* prototypes */

GtkWidget * init_kemoview_colormap_list_vbox(struct kemoviewer_gl_type *kemo_gl,
                                             struct colormap_view *color_vws);
	
#endif /* TREE_VIEW_KEMOVIEW_COLORMAP_ */
