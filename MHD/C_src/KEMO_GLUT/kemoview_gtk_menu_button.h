/*
 *  kemoview_gtk_menu_button.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_MENU_BUTTON_
#define KEMOVIEW_GTK_MENU_BUTTON_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_GTK.h"
#include "kemoviewer.h"
#include "kemoview_gtk_routines.h"
#include "kemoview_gtk_main_menu.h"
#include "kemoview_gtk_preference_menu.h"
#include "kemoview_gtk_evolution_menu.h"
#include "view_modifier_glfw.h"

/*  prototypes */

GtkWidget *make_gtk_menu_button(struct kemoviewer_gl_type *kemo_gl,
                                GtkWidget *main_window, 
                                struct main_buttons *mbot);

#endif /* KEMOVIEW_GTK_MENU_BUTTON_ */
