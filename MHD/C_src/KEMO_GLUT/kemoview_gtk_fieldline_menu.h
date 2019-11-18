/*
 *  kemoview_gtk_fieldline_menu.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_FIELDLINE_MENU_
#define KEMOVIEW_GTK_FIELDLINE_MENU_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_GTK.h"
#include "kemoviewer.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "kemoview_gtk_fline_selectors.h"

#ifdef GLFW3
	#include "view_modifier_glfw.h"
#else
	#include "view_modifier_gtk.h"
#endif

struct fieldline_gtk_menu{
	GtkWidget *menu_box;
	
	GtkWidget *combobox_color;
	GtkWidget *switch_tube;
	
	GtkWidget *label_min, *label_max;
	GtkWidget *spin_range_min, *spin_min_digit;
	GtkWidget *spin_range_max, *spin_max_digit;
	GtkWidget *spin_thick, *spin_digit;
};


/*  prototypes */

void set_gtk_fieldline_menu(struct fieldline_gtk_menu *fline_menu);
void add_gtk_fieldline_menu(struct fieldline_gtk_menu *fline_menu);

#endif
