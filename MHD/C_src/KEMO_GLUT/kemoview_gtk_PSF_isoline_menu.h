/*
 *  kemoview_gtk_PSF_isoline_menu.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_PSF_ISOLINE_MENU_
#define KEMOVIEW_GTK_PSF_ISOLINE_MENU_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_GTK.h"
#include "kemoviewer.h"
#include "m_kemoviewer_data.h"
#include "tree_view_4_pvr_colormap.h"
#include "tree_view_chara_int_GTK.h"
#include "kemoview_gtk_routines.h"

#ifdef GLFW3
	#include "view_modifier_glfw.h"
#else
	#include "view_modifier_gtk.h"
#endif

struct gtk_psf_isoline_menu{
	GtkWidget *hbox_draw, *hbox_zero, *hbox_color;
	GtkWidget *hbox_nline, *hbox_width;
	
	GtkWidget *vbox_iso;
	
	GtkWidget *switch_1, *switch_zero;
	
	GtkWidget *combobox_gdcolor;
	GtkWidget *label_tree_gdcolor;
	GtkCellRenderer *renderer_gdcolor;
	GtkTreeModel *model_gdcolor;
	GtkTreeModel *child_model_gdcolor;
	
	int index;
	int iflag_sfcolor;
	
	GtkWidget *spin_nline, *spin_width;
	GtkAdjustment *adj_nline, *adj_width;
};


/*  prototypes */
void add_gtk_isoline_menu(GtkWidget *window, GtkWidget *box);
	
#endif
