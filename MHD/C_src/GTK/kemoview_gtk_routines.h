/*
 *  kemoview_gtk_routines.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_ROUTINES_
#define KEMOVIEW_GTK_ROUTINES_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_GTK.h"
#include "tree_views_4_fixed_lists_GTK.h"

/*  prototypes */

GtkWidget * wrap_into_frame_gtk(const char *title, GtkWidget *box_in);
GtkWidget * wrap_into_expanded_frame_gtk(const char *title, int width, int height, 
                                  GtkWidget *window, GtkWidget *box_in);

int gtk_selected_combobox_index(GtkComboBox *combobox);


#endif
