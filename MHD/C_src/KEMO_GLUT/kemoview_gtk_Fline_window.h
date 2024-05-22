/*
 *  kemoview_gtk_Fline_window.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_FLINE_WINDOW_
#define KEMOVIEW_GTK_FLINE_WINDOW_

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
#include "kemoview_gtk_fieldline_menu.h"

/* prototypes */

void init_fline_window(struct kemoviewer_type *kemo_sgl,
                       struct fieldline_gtk_menu *fline_menu,
                       GtkWidget *window);

#endif /*  KEMOVIEW_GTK_FLINE_WINDOW_  */
