/*
 *  kemoview_gtk_evolution_menu.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_EVOLUTION_MENU_
#define KEMOVIEW_GTK_EVOLUTION_MENU_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <gtk/gtk.h>

#include "kemoviewer.h"
#include "m_kemoviewer_data.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "kemoview_fileselector_gtk.h"
#include "kemoview_gtk_routines.h"

/*  prototypes */

void add_evoluaiton_menu_box(struct kemoviewer_type *kemoviewer_data, 
			GtkWidget *window_main, GtkWidget *box_out);
	
#endif
