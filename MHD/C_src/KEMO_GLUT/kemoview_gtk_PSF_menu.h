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

struct psf_color_gtk_menu{
	GtkWidget *switch_draw, *switch_bar;
	
	GtkWidget *combobox_sfcolor;
	
	GtkWidget *spin_opacity1;	
	GtkWidget *spin_range_min, *spin_digit_min; 
	GtkWidget *spin_range_max, *spin_digit_max;
	char min_text[30], max_text[30];
};

struct psf_gtk_menu{
	GtkWidget *psf_vbox;
	
	GtkWidget *closeButton;
	GtkWidget *combobox_field;
	GtkWidget *combobox_comp;
	
	struct psf_isoline_gtk_menu *psf_isoline_menu;
	struct psf_surface_gtk_menu *psf_surface_menu;
	struct psf_color_gtk_menu *psf_color_menu;
	struct psf_vector_gtk_menu *psf_vector_menu;
	
	struct colormap_view *color_vws;
};

/*  prototypes */

struct psf_gtk_menu * alloc_psf_gtk_menu();
void dealloc_psf_gtk_menu(struct psf_gtk_menu *psf_gmenu);

GtkWidget * init_psf_menu_hbox(struct psf_gtk_menu *psf_gmenu, 
                               GtkWidget *window, GtkWidget *psf_vbox);

#endif
