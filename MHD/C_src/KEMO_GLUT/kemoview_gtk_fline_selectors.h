/*
 *  kemoview_gtk_fline_selectors.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_FLINE_SELECTORS_
#define KEMOVIEW_GTK_FLINE_SELECTORS_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_GTK.h"
#include "kemoviewer.h"
#include "m_kemoviewer_data.h"
#include "m_kemoview_psf_menu.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_chara_int_GTK.h"
#include "kemoview_gtk_routines.h"

#ifdef GLFW3
	#include "view_modifier_glfw.h"
#else
	#include "view_modifier_gtk.h"
#endif

/*  prototypes */

GtkWidget * fline_draw_field_box(struct kemoviewer_type *kemo_sgl,
                                 GtkWidget *label_tree_field,
                                 GtkCellRenderer *renderer_field);

GtkWidget * fline_draw_component_combobox(struct kemoviewer_type *kemo_sgl,
                                          GtkWidget *label_tree_comp,
                                          GtkCellRenderer *renderer_comp);
#endif
