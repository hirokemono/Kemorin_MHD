/*
 *  kemoview_gtk_shading_mode_menu.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_SHADING_MODE_MENU_
#define KEMOVIEW_GTK_SHADING_MODE_MENU_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_GTK.h"
#include "kemoviewer_gl.h"
#include "kemoview_gtk_routines.h"
#include "tree_view_chara_int_GTK.h"

#include "view_modifier_glfw.h"

GtkWidget * shading_mode_menu_frame(struct kemoviewer_gl_type *kemo_gl);

#endif /*KEMOVIEW_GTK_SHADING_MODE_MENU_ */
