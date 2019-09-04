/*
 *  kemoview_gtk_psf_selectors.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_PSF_SELECTORS_
#define KEMOVIEW_GTK_PSF_SELECTORS_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <gtk/gtk.h>

#include "kemoviewer.h"
#include "m_kemoviewer_data.h"
#include "m_kemoview_psf_menu.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_4_pvr_colormap.h"

/*  prototypes */

void add_current_psf_set_box(struct kemoviewer_type *kemoviewer_data,
			GtkWidget *window_cmap, GtkWidget *box);
void add_psf_draw_field_box(struct kemoviewer_type *kemoviewer_data,
			GtkWidget *window_cmap, GtkWidget *box);
void add_psf_draw_component_box(struct kemoviewer_type *kemoviewer_data,
			GtkWidget *window_cmap, GtkWidget *box);
#endif
