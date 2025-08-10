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
#include "kemoview_gtk_fileselector.h"
#include "kemoview_gtk_PSF_menu.h"
#include "kemoview_gtk_evolution_menu.h"
#include "kemoview_gtk_fline_selectors.h"

#include "view_modifier_glfw.h"


/*  prototypes */
void replace_psf_menu_frame(struct kemoviewer_gl_type *kemo_gl,
                            struct psf_gtk_menu *psf_gmenu,
                            GtkWidget *itemTEvo);

void init_psf_window(struct kemoviewer_gl_type *kemo_gl,
                     struct psf_gtk_menu *psf_gmenu,
                     GtkWidget *main_window, GtkWidget *itemTEvo);

#endif /*  KEMOVIEW_GTK_PSF_WINDOW_ */
