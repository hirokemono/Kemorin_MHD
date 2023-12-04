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
#include "kemoview_fileselector_gtk.h"
#include "kemoview_gtk_evolution_menu.h"
#include "kemoview_gtk_rotation_menu.h"
#include "kemoview_gtk_quilt_menu.h"
#include "kemoview_gtk_axis_menu.h"
#include "kemoview_gtk_preference_menu.h"
#include "kemoview_gtk_PSF_menu.h"
#include "kemoview_gtk_fieldline_menu.h"
#include "kemoview_gtk_viewmatrix_menu.h"
#include "kemoview_gtk_mesh_menu.h"

#ifdef GLFW3
	#include "view_modifier_glfw.h"
#else
	#include "view_modifier_gtk.h"
#endif

struct updatable_widgets{
    GtkWidget *psfBox;
    struct psf_gtk_menu *psf_gmenu;
    
    GtkWidget *flineBox;
    struct fieldline_gtk_menu *fline_menu;
    
    GtkWidget *meshBox;
    struct kemoview_mesh_view *mesh_vws;
    
    int itep_current;
    GtkWidget *evolutionBox;
    struct evolution_gtk_menu *evo_gmenu;
};

struct main_buttons{
	GtkWidget *menuHbox;
	
	GtkWidget *vbox_menu;

    struct updatable_widgets *updatable;
    GtkWidget *expander_view;
    struct view_widgets *view_menu;
	
    GtkWidget *expander_pref;
	struct preference_gtk_menu  *pref_gmenu;

	struct rotation_gtk_menu *rot_gmenu;
    
    GtkWidget *expander_quilt;
	struct quilt_gtk_menu *quilt_gmenu;

	GtkWidget *ComboboxImageFormat;
	int id_iamge_format;
};


/*  prototypes */
struct main_buttons * init_main_buttons(struct kemoviewer_type *kemoviewer_data);
void dealloc_main_buttons(struct main_buttons *mbot);

void open_kemoviewer_file_glfw(struct kv_string *filename, struct main_buttons *mbot,
							   GtkWidget *window_main);

void gtk_psf_menu_box(struct updatable_widgets *updatable, GtkWidget *menuHbox,
                      GtkWidget *window);
GtkWidget * pack_psf_menu_frame(struct psf_gtk_menu *psf_gmenu);

void gtk_fieldline_menu_box(struct updatable_widgets *updatable, GtkWidget *menuHbox,
                            GtkWidget *window);

void gtk_mesh_menu_box(struct updatable_widgets *updatable,
                       GtkWidget *menuHbox, GtkWidget *window);

void update_kemoview_menu(struct updatable_widgets *updatable, GtkWidget *menuHbox,
                          GtkWidget *window);
void pack_kemoview_menu(struct updatable_widgets *updatable,
                        GtkWidget *menuHbox, GtkWidget *window);

void make_gtk_main_menu_box(struct main_buttons *mbot, GtkWidget *takobox,
                            GtkWidget *window_main,
                            struct kemoviewer_type *kemoviewer_data);


void sel_mesh_menu_box(struct main_buttons *mbot, gboolean flag);

#endif
