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

#include "calypso_GTK.h"
#include "kemoviewer.h"
#include "m_kemoviewer_data.h"
#include "m_kemoview_psf_menu.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_chara_int_GTK.h"
#include "tree_view_4_colormap.h"
#include "tree_view_kemoview_colormap.h"
#include "tree_view_4_light_position.h"
#include "kemoview_fileselector_gtk.h"
#include "kemoview_gtk_PSF_surface_menu.h"
#include "kemoview_gtk_PSF_isoline_menu.h"
#include "kemoview_gtk_PSF_vector_menu.h"

#ifdef GLFW3
	#include "view_modifier_glfw.h"
#else
	#include "view_modifier_gtk.h"
#endif

struct gtk_psf_color_menu{
	GtkWidget *switch_draw, *switch_bar;
	
	GtkWidget *combobox_sfcolor;
	
	GtkWidget *spin_opacity1;	
	GtkWidget *spin_range_min, *spin_digit_min; 
	GtkWidget *spin_range_max, *spin_digit_max;
	char min_text[30], max_text[30];	
};


/*  prototypes */

void add_gtk_psf_colormap_menu(struct colormap_view *color_vws, GtkWidget *window, 
			struct gtk_psf_color_menu *gtk_psf_color, GtkWidget *box);

void make_psf_menu_box(struct colormap_view *color_vws,
					   GtkWidget *window, GtkWidget *box_out);

#endif
