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

#include "calypso_GTK.h"
#include "kemoviewer.h"
#include "m_kemoviewer_data.h"
#include "tree_view_chara_int_GTK.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "kemoview_fileselector_gtk.h"
#include "kemoview_gtk_routines.h"

#ifdef GLFW3
	#include "view_modifier_glfw.h"
#else
	#include "view_modifier_gtk.h"
#endif

struct evolution_gtk_menu{
	int id_fmt_evo;
	
	int istart_evo;
	int iend_evo;
	int inc_evo;
	
	GtkWidget *spin_evo_start;
	GtkWidget *spin_evo_end;
	GtkWidget *spin_evo_increment;
	
	GtkWidget *combobox_evo_fileformat;
};

/*  prototypes */

struct evolution_gtk_menu * init_evoluaiton_menu_box(void);
GtkWidget * init_evoluaiton_menu_expander(int istep, GtkWidget *window, struct evolution_gtk_menu *evo_gmenu);
	
#endif
