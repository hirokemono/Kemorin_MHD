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

#include "gtk/gtk.h"

#include "kemoviewer.h"
#include "m_kemoviewer_data.h"
#include "tree_view_4_pvr_colormap.h"

/*  prototypes */
void add_gtk_isoline_menu(struct colormap_view *color_vws, 
			GtkWidget *window_cmap, GtkWidget *box);
	
#endif
