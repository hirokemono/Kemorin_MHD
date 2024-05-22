/*
 *  kemoview_gtk_axis_prefs_menu.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#ifndef KEMOVIEW_GTK_AXIS_PREFS_MENU_
#define KEMOVIEW_GTK_AXIS_PREFS_MENU_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_GTK.h"
#include "kemoviewer_gl.h"
#include "tree_view_chara_int_GTK.h"
#include "kemoview_gtk_routines.h"

#ifdef GLFW3
	#include "view_modifier_glfw.h"
#else
	#include "view_modifier_gtk.h"
#endif

/* protytypes */

GtkWidget * init_coastline_pref_menu(struct kemoviewer_type *kemo_sgl);
GtkWidget * init_axis_position_menu(struct kemoviewer_type *kemo_sgl);

#endif /* KEMOVIEW_GTK_AXIS_PREFS_MENU_*/
