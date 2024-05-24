/*
 *  kemoview_gtk_main_menu.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_MAIN_MENU_
#define KEMOVIEW_GTK_MAIN_MENU_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_GTK.h"
#include "kemoviewer.h"
#include "kemoviewer_gl.h"
#include "m_kemoviewer_data.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "tree_view_viewer_colormap.h"
#include "tree_view_4_light_position.h"
#include "kemoview_gtk_fileselector.h"
#include "kemoview_gtk_evolution_menu.h"
#include "kemoview_gtk_rotation_menu.h"
#include "kemoview_gtk_quilt_menu.h"
#include "kemoview_gtk_axis_menu.h"
#include "kemoview_gtk_preference_menu.h"
#include "kemoview_gtk_viewmatrix_menu.h"
#include "kemoview_gtk_mesh_menu.h"
#include "kemoview_gtk_PSF_window.h"
#include "kemoview_gtk_Fline_window.h"
#include "kemoview_gtk_viewmode_menu.h"
#include "kemoview_gtk_menu_button.h"

#include "view_modifier_glfw.h"


struct main_buttons{
    int id_current[1];
    struct psf_gtk_menu *psf_gmenu;
    struct fieldline_gtk_menu *fline_menu;
    
    GtkWidget *meshWin;
    struct kemoview_mesh_view *mesh_vws;
    
    GtkWidget *itemTEvo;
    struct evolution_gtk_menu *evo_gmenu;

    GtkWidget *expander_view;
    struct view_widgets *view_menu;
	
    GtkWidget *expander_pref;
    struct lightparams_view *lightparams_vws;

	struct rotation_gtk_menu *rot_gmenu;
    
    GtkWidget *expander_quilt;
	struct quilt_gtk_menu *quilt_gmenu;
};


/*  prototypes */
struct main_buttons * init_main_buttons(struct kemoviewer_type *kemoviewer_data);
void dealloc_main_buttons(struct main_buttons *mbot);

void open_kemoviewer_file_glfw(struct kemoviewer_type *kemo_sgl,
                               struct kemoviewer_gl_type *kemo_gl,
                               struct kv_string *filename, 
                               struct main_buttons *mbot);

GtkWidget * make_gtk_main_menu_box(struct main_buttons *mbot,
                                   GtkWidget *quitButton, GtkWidget *window_main,
                                   struct kemoviewer_type *kemo_sgl,
                                   struct kemoviewer_gl_type *kemo_gl);

#endif
