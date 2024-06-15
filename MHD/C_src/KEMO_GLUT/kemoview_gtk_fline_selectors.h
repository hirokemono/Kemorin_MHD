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
#include "kemoviewer_gl.h"
#include "m_kemoviewer_data.h"
#include "m_kemoview_psf_menu.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_chara_int_GTK.h"
#include "kemoview_gtk_routines.h"

#include "view_modifier_glfw.h"

/*  prototypes */

void kemoview_psf_select_CB(GtkComboBox *combobox_psfs, int id_model,
                            struct kemoviewer_gl_type *kemo_gl);
void kemoview_field_select_CB(GtkComboBox *combobox_field, int id_model,
                              struct kemoviewer_gl_type *kemo_gl);
void kemoview_component_select_CB(GtkComboBox *combobox_comp, int id_model,
                                  struct kemoviewer_gl_type *kemo_gl);

GtkWidget * draw_current_psf_set_hbox(int id_current_psf,
                                      struct kemoviewer_gl_type *kemo_gl,
                                      int *index);
GtkWidget * draw_viz_field_gtk_box(struct kemoviewer_gl_type *kemo_gl, int id_model);
GtkWidget * draw_viz_component_gtk_box(struct kemoviewer_gl_type *kemo_gl, int id_model);
#endif
