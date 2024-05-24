/*
 *  kemoview_gtk_axis_menu.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_AXIS_MENU_
#define KEMOVIEW_GTK_AXIS_MENU_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_GTK.h"
#include "kemoviewer.h"
#include "m_kemoviewer_data.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_chara_int_GTK.h"
#include "kemoview_gtk_fileselector.h"
#include "kemoview_gtk_routines.h"

#include "view_modifier_glfw.h"

/*  prototypes */

GtkWidget * make_axis_menu_box(struct kemoviewer_type *kemo_sgl,
                               GtkWidget *window);

#endif
