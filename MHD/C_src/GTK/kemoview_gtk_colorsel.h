/*
 *  kemoview_gtk_colorsel.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_COLORSEL_
#define KEMOVIEW_GTK_COLORSEL_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_GTK.h"

/*  prototypes */

void set_color_to_GTK(float color[4], GdkRGBA *gcolor);
int kemoview_gtk_colorsel_CB(GtkWindow *parent_win, float color[4]);

#endif
