/*
 *  kemoview_gtk_preference_menu.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_PREFERENCE_MENU_
#define KEMOVIEW_GTK_PREFERENCE_MENU_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_GTK.h"
#include "kemoviewer_gl.h"
#include "m_kemoviewer_data.h"
#include "tree_view_4_pvr_colormap.h"
#include "kemoview_gtk_colorsel.h"
#include "kemoview_gtk_routines.h"
#include "kemoview_gtk_rotation_menu.h"
#include "kemoview_gtk_shading_mode_menu.h"
#include "kemoview_gtk_tube_pref_menu.h"
#include "kemoview_gtk_axis_prefs_menu.h"
#include "kemoview_gtk_performance_menu.h"
#include "kemoview_gtk_lighting_menu.h"

#include "view_modifier_glfw.h"

/*  prototypes */
GtkWidget * init_preference_frame(struct kemoviewer_gl_type *kemo_gl,
                                  struct lightparams_view *lightparams_vws,
                                  GtkWidget *window);
GtkWidget * init_preference_scrollbox(struct kemoviewer_gl_type *kemo_gl,
                                     struct lightparams_view *lightparams_vws,
                                      GtkWidget *window);
GtkWidget * init_preference_expander(struct kemoviewer_gl_type *kemo_gl,
                                     struct lightparams_view *lightparams_vws,
                                     GtkWidget *window);

#endif
