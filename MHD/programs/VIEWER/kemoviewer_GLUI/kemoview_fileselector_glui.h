/*
 *  kemoview_fileselector_glui.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_FILESELECTOR_GLUI_
#define KEMOVIEW_FILESELECTOR_GLUI_

#include <string.h>
#include <unistd.h>
#include <GL/glui.h>

#include "kemoviewer.h"
#include "kemoview_glut_routines.h"

/* prototypes */

void myGlutIdle( void );
void reset_glui_idle_count();
int send_glui_idle_count();

void set_saveimage_menu_glui(int winid);
void set_evolution_menu_glui(int winid);
void set_rotateimages_menu_glui(int winid);

void save_viewmatrix_file_glui(int winid);

#endif
