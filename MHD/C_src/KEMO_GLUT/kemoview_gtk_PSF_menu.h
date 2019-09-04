/*
 *  kemoview_gtk_PSF_menu.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_PSF_MENU_
#define KEMOVIEW_GTK_PSF_MENU_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <gtk/gtk.h>

#include "kemoviewer.h"
#include "m_kemoviewer_data.h"
#include "tree_view_4_pvr_colormap.h"
#include "tree_view_4_light_position.h"
#include "kemoview_fileselector_gtk.h"
#include "kemoview_gtk_psf_selectors.h"
#include "kemoview_gtk_PSF_surface_menu.h"
#include "kemoview_gtk_PSF_isoline_menu.h"
#include "kemoview_gtk_PSF_vector_menu.h"

#include "view_modifier_glfw.h"

/*  prototypes */

void gtk_psf_colormap_menu(struct kemoviewer_type *kemoviewer_data);

#endif
