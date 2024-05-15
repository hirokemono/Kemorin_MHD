/*
 *  kemoview_gtk_rotation_menu.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_ROTATION_MENU_
#define KEMOVIEW_GTK_ROTATION_MENU_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_GTK.h"
#include "kemoviewer.h"
#include "kemoviewer_gl.h"
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

struct rotation_gtk_menu{
	int id_fmt_rot;
	
	int inc_deg;
	int iaxis_rot;
		
	GtkWidget *combobox_rotation_dir;
	GtkWidget *spin_rot_increment;
	
	GtkWidget *combobox_rotation_fileformat;
	GtkWidget *rotView_Button;
	GtkWidget *rotSave_Button;
};


/*  prototypes */

struct rotation_gtk_menu * init_rotation_menu_box(void);
GtkWidget * init_rotation_menu_expander(struct kemoviewer_type *kemo_sgl,
                                        struct rotation_gtk_menu *rot_gmenu,
                                        GtkWidget *window);

#endif
