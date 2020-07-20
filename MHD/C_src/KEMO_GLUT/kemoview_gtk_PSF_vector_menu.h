/*
 *  kemoview_gtk_PSF_vector_menu.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_PSF_VECTOR_MENU_
#define KEMOVIEW_GTK_PSF_VECTOR_MENU_

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

struct psf_vector_gtk_menu{
	GtkWidget *vector_box;
	
	GtkWidget *switch_vect;
	
	GtkWidget *combobox_vecmode;
	GtkWidget *combobox_veccolor;
	
	GtkWidget *spin_ref_vect, *spin_ref_digit;
	GtkWidget *spin_vect_inc, *spin_inc_digit;
	GtkWidget *spin_vect_width, *spin_width_digit;
};


/*  prototypes */

void set_gtk_psf_vector_menu(struct psf_vector_gtk_menu *psf_vector_menu);
GtkWidget * make_gtk_psf_vector_menu(GtkWidget *window, struct psf_vector_gtk_menu *psf_vector_menu);

#endif
