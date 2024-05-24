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

#include "view_modifier_glfw.h"

struct psf_isoline_gtk_menu{
	GtkWidget *switch_1, *switch_zero;
	
	GtkWidget *combobox_gdcolor;
	GtkWidget *label_tree_gdcolor;
	GtkCellRenderer *renderer_gdcolor;
	GtkTreeModel *model_gdcolor;
	GtkTreeModel *child_model_gdcolor;
	
	GtkWidget *spin_nline, *spin_width, *spin_digit;
};


/*  prototypes */
void set_gtk_isoline_menu_values(struct kemoviewer_type *kemo_sgl, 
                                 struct psf_isoline_gtk_menu *psf_isoline_menu);
GtkWidget * init_isoline_menu_expander(struct kemoviewer_type *kemo_sgl, GtkWidget *window,
                                       struct psf_isoline_gtk_menu *psf_isoline_menu);

#endif
