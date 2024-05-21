/*
 *  kemoview_gtk_PSF_window.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_PSF_WINDOW_
#define KEMOVIEW_GTK_PSF_WINDOW_

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
#include "kemoview_gtk_PSF_menu.h"
#include "view_modifier_glfw.h"

#ifdef GLFW3
	#include "view_modifier_glfw.h"
#else
	#include "view_modifier_gtk.h"
#endif


/*  prototypes */

void init_psf_window(struct kemoviewer_type *kemo_sgl,
                     struct kemoviewer_gl_type *kemo_gl,
                     struct psf_gtk_menu *psf_gmenu,
                     GtkWidget *window);

#endif /*  KEMOVIEW_GTK_PSF_WINDOW_ */
