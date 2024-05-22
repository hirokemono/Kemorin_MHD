/*
 *  kemoview_gtk_viewmode_menu.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_VIEWMODE_MENU_
#define KEMOVIEW_GTK_VIEWMODE_MENU_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_GTK.h"
#include "kemoviewer.h"
#include "kemoviewer_gl.h"
#include "m_kemoviewer_data.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_viewer_colormap.h"
#include "kemoview_fileselector_gtk.h"
#include "kemoview_gtk_viewmatrix_menu.h"

#include "view_modifier_glfw.h"

/* prototypes */

GtkWidget * make_gtk_viewmode_menu_box(struct view_widgets *view_menu,
                                       struct kemoviewer_type *kemo_sgl);

#endif /*  KEMOVIEW_GTK_VIEWMODE_MENU_  */
