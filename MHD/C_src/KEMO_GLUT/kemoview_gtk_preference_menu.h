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

#include <gtk/gtk.h>

#include "kemoviewer.h"
#include "m_kemoviewer_data.h"
#include "tree_view_4_pvr_colormap.h"
#include "tree_view_4_light_position.h"
#include "kemoview_gtk_colorsel.h"

/*  prototypes */

void kemoview_preference_GTK(struct kemoviewer_type *kemoviewer_data);
	
#endif
