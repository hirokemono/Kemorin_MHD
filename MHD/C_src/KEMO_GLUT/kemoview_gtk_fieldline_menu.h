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

#include "view_modifier_glfw.h"

struct fieldline_gtk_menu{
    int iflag_flineBox;
    GtkWidget *flineWin;

    GtkWidget *closeButton;
    GtkWidget *combobox_color;
    GtkWidget *switch_tube;
    
    GtkWidget *label_min, *label_max;
    GtkWidget *spin_range_min, *spin_min_digit;
    GtkWidget *spin_range_max, *spin_max_digit;
    GtkWidget *spin_thick, *spin_digit;
    
    GtkWidget *combobox_field;
    GtkWidget *label_tree_field;
    GtkCellRenderer *renderer_field;

    GtkWidget *combobox_comp;
    GtkWidget *label_tree_comp;
    GtkCellRenderer *renderer_comp;
};


/*  prototypes */

void set_gtk_fieldline_menu(struct kemoviewer_gl_type *kemo_gl,
                            struct fieldline_gtk_menu *fline_menu);
void init_fieldline_menu_hbox(struct kemoviewer_gl_type *kemo_gl,
                              struct fieldline_gtk_menu *fline_menu);

GtkWidget * pack_fieldline_menu_frame(struct fieldline_gtk_menu *fline_menu);

#endif
