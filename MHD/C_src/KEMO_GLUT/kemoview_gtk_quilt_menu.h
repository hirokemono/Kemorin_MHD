/*
 *  kemoview_gtk_quilt_menu.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_QUILT_MENU_
#define KEMOVIEW_GTK_QUILT_MENU_

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
#include "view_modifier_glfw.h"

#ifdef GLFW3
	#include "view_modifier_glfw.h"
#else
	#include "view_modifier_gtk.h"
#endif

struct quilt_gtk_menu{
	int id_fmt_quilt;
	
	int num_column;
	int num_raw;
		
	GtkWidget *combobox_rotation_dir;
	GtkWidget *spin_num_column;
	GtkWidget *spin_num_raw;
	
	GtkWidget *quiltOn_Switch;
	GtkWidget *quiltView_Button;
	
	GtkWidget *entry_quilt_menu;
};


/*  prototypes */

struct quilt_gtk_menu * init_quilt_menu_box(void);
GtkWidget * init_quilt_menu_expander(struct quilt_gtk_menu *quilt_gmenu, GtkWidget *window);
	
#endif
