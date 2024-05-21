/*
 *  kemoview_gtk_performance_menu.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_PERFORMANCE_MENU_
#define KEMOVIEW_GTK_PERFORMANCE_MENU_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_GTK.h"
#include "kemoviewer_gl.h"
#include "m_kemoviewer_data.h"
#include "kemoview_gtk_routines.h"
#include "view_modifier_glfw.h"

#ifdef GLFW3
	#include "view_modifier_glfw.h"
#else
	#include "view_modifier_gtk.h"
#endif

/*  prototypes */

GtkWidget * init_num_threads_menu_frame(struct kemoviewer_type *kemo_sgl);
GtkWidget * init_FPS_test_menu_frame(struct kemoviewer_type *kemo_sgl,
                                     GtkWidget *window);

#endif /*  KEMOVIEW_GTK_PERFORMANCE_MENU_  */
