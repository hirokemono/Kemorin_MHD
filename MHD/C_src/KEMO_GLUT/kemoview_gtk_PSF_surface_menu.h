/*
 *  kemoview_gtk_PSF_surface_menu.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_PSF_SURFACE_MENU_
#define KEMOVIEW_GTK_PSF_SURFACE_MENU_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_GTK.h"
#include "kemoviewer.h"
#include "kemoviewer_gl.h"
#include "m_kemoviewer_data.h"
#include "tree_view_4_pvr_colormap.h"
#include "tree_view_chara_int_GTK.h"
#include "kemoview_gtk_routines.h"
#include "kemoview_fileselector_gtk.h"

#ifdef GLFW3
	#include "view_modifier_glfw.h"
#else
	#include "view_modifier_gtk.h"
#endif

struct psf_surface_gtk_menu{
	GtkWidget *switch_draw, *switch_bar;
	
	GtkWidget *combobox_sfcolor;
	
	GtkWidget *label_range_min, *label_range_max;
	GtkWidget *spin_opacity1;
	GtkWidget *spin_range_min, *spin_digit_min; 
	GtkWidget *spin_range_max, *spin_digit_max;
};


/*  prototypes */

void set_gtk_surface_menu_values(struct kemoviewer_type *kemo_sgl,
                                 struct psf_surface_gtk_menu *gtk_psf_surface);
GtkWidget * init_gtk_psf_surface_menu_expander(struct kemoviewer_type *kemo_sgl,
                                               struct kemoviewer_gl_type *kemo_gl,
                                               GtkWidget *window, 
                                               struct colormap_view *color_vws, 
                                               struct psf_surface_gtk_menu *psf_surface_menu);
#endif
