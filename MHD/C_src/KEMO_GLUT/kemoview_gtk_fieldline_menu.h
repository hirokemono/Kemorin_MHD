/*
 *  kemoview_gtk_fieldline_menu.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_FIELDLINE_MENU_
#define KEMOVIEW_GTK_FIELDLINE_MENU_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_GTK.h"
#include "kemoviewer_gl.h"
#include "tree_views_4_fixed_lists_GTK.h"
#include "kemoview_gtk_fline_selectors.h"
#include "tree_view_viewer_colormap.h"
#include "kemoview_gtk_PSF_menu.h"

#include "view_modifier_glfw.h"

struct fieldline_gtk_menu{
    int iflag_flinemode;
    
    int iflag_flineBox;
    GtkWidget *flineWin;
    
    GtkWidget *closeButton;
    GtkWidget *combobox_color;
    GtkWidget *switch_tube;
    GtkWidget *fline_switch_bar;
    
    GtkWidget *label_min, *label_max;
    GtkWidget *spin_range_min, *spin_min_digit;
    GtkWidget *spin_range_max, *spin_max_digit;
    GtkWidget *spin_thick, *spin_digit;
    
    GtkWidget *combobox_field;
    GtkWidget *label_tree_field;
    
    GtkWidget *combobox_comp;
    
    struct colormap_view *fline_color_vws;
    GtkWidget *expander_fline_color;
    
    GtkWidget *fline_frame;
    
    GtkWidget *switch_vect;
    GtkWidget *spin_vect_inc;
    GtkWidget *spin_inc_digit;
    GtkWidget *spin_ref_vect;
    GtkWidget *spin_ref_digit;

};


/*  prototypes */

void set_gtk_fieldline_menu(struct kemoviewer_gl_type *kemo_gl,
                            struct fieldline_gtk_menu *fline_gmenu);
void init_fieldline_menu_hbox(struct kemoviewer_gl_type *kemo_gl,
                              struct fieldline_gtk_menu *fline_gmenu);
GtkWidget * pack_fieldline_menu_frame(struct fieldline_gtk_menu *fline_gmenu);

void init_tracer_menu_hbox(struct kemoviewer_gl_type *kemo_gl,
                           struct fieldline_gtk_menu *fline_gmenu);
GtkWidget * pack_tracer_menu_frame(struct fieldline_gtk_menu *fline_gmenu);

#endif
