/*
 *  kemoview_gtk_preference_menu.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_PREFERENCE_MENU_
#define KEMOVIEW_GTK_PREFERENCE_MENU_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_GTK.h"
#include "kemoviewer.h"
#include "m_kemoviewer_data.h"
#include "tree_view_4_pvr_colormap.h"
#include "tree_view_4_light_position.h"
#include "kemoview_gtk_colorsel.h"
#include "kemoview_gtk_routines.h"

#ifdef GLFW3
	#include "view_modifier_glfw.h"
#else
	#include "view_modifier_gtk.h"
#endif

struct preference_gtk_menu{
	struct lightparams_view *lightparams_vws;
	
	GtkWidget *box_pref;
	
	GtkWidget *spin_ambient;
	GtkWidget *spin_diffuse;
	GtkWidget *spin_specular;
	GtkWidget *spin_shineness;

	GtkWidget *combobox_node_color;
	GtkWidget *button_node_color;
	GdkRGBA gcolor;
};


/*  prototypes */

struct preference_gtk_menu * init_preference_gtk_menu(struct kemoviewer_type *kemoviewer_data);
void dealloc_preference_gtk_menu(struct preference_gtk_menu *pref_gmenu);

void set_GTK_preference_menu(struct preference_gtk_menu *pref_gmenu);
void add_GTK_preference_box(struct preference_gtk_menu *pref_gmenu);

#endif
