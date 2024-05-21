/*
 *  kemoview_gtk_lighting_menu.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_LIGHTING_MENU_
#define KEMOVIEW_GTK_LIGHTING_MENU_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_GTK.h"
#include "kemoviewer.h"
#include "kemoviewer_gl.h"
#include "m_kemoviewer_data.h"
#include "tree_view_4_light_position.h"
#include "kemoview_gtk_routines.h"

#ifdef GLFW3
	#include "view_modifier_glfw.h"
#else
	#include "view_modifier_gtk.h"
#endif

GtkWidget * init_lighting_frame(struct kemoviewer_type *kemo_sgl,
                                struct lightparams_view *lightparams_vws);

#endif /* KEMOVIEW_GTK_LIGHTING_MENU_ */
