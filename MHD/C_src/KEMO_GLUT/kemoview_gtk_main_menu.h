/*
 *  kemoview_gtk_main_menu.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_MAIN_MENU_
#define KEMOVIEW_GTK_MAIN_MENU_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <gtk/gtk.h>

#include "kemoviewer.h"
#include "m_kemoviewer_data.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "kemoview_fileselector_gtk.h"
#include "kemoview_gtk_evolution_menu.h"
#include "kemoview_gtk_rotation_menu.h"
#include "kemoview_gtk_axis_menu.h"
#include "kemoview_gtk_preference_menu.h"
#include "kemoview_gtk_PSF_menu.h"
#include "kemoview_gtk_fieldline_menu.h"
#include "kemoview_gtk_viewmatrix_menu.h"

#include "view_modifier_glfw.h"

struct main_buttons{
	GtkWidget *mainHbox;
	
	GtkWidget *BaseBox;
	GtkWidget *psfBox;
	
	GtkWidget *prefButton;
	GtkWidget *flineButton;
	GtkWidget *meshButton;
	
	GtkWidget *rotationBox;
	GtkWidget *evolutionBox;
};


/*  prototypes */

void gtk_psf_menu_box(struct kemoviewer_type *kemoviewer_data, struct colormap_view *color_vws,
			struct main_buttons *mbot, GtkWidget *window, GtkWidget *box_out);

void make_gtk_main_menu_box(struct kemoviewer_type *kemoviewer_data,
					   struct view_widgets *view_menu, GtkWidget *window_main, GtkWidget *box);
	
#endif
