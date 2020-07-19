/*
 *  kemoview_gtk_viewmatrix_menu.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_VIEWMATRIX_MENU_
#define KEMOVIEW_GTK_VIEWMATRIX_MENU_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_GTK.h"
#include "kemoviewer.h"
#include "m_kemoviewer_data.h"

#include "kemoview_gtk_routines.h"
#include "kemoview_fileselector_gtk.h"

#ifdef GLFW3
	#include "view_modifier_glfw.h"
#else
	#include "view_modifier_gtk.h"
#endif



struct view_widgets{
	GtkAdjustment *adj_win_x, *adj_win_y;
	GtkWidget *spin_win_x, *spin_win_y;
	GtkWidget *hbox_win_x, *hbox_win_y;
	
	GtkAdjustment *adj_eye_x, *adj_eye_y, *adj_eye_z;
	GtkWidget *spin_eye_x, *spin_eye_y, *spin_eye_z;
	GtkWidget *hbox_eye_x, *hbox_eye_y, *hbox_eye_z;
	
	GtkWidget *hbox_looking_x, *hbox_looking_y, *hbox_looking_z;
	
	GtkAdjustment *adj_scale;
	GtkWidget *spin_scale;
	GtkWidget *hbox_scale;
	
	GtkAdjustment *adj_rotation_x, *adj_rotation_y, *adj_rotation_z;
	GtkWidget *spin_rotation_x, *spin_rotation_y, *spin_rotation_z;
	GtkWidget *hbox_rotation_x, *hbox_rotation_y, *hbox_rotation_z;
	
	GtkAdjustment *adj_rotation_deg;
	GtkWidget *spin_rotation_deg;
	GtkWidget *hbox_rotation_deg;
	
	GtkAdjustment *adj_aperture;
	GtkWidget *spin_aperture;
	GtkWidget *hbox_aperture;
	
	GtkAdjustment *adj_focus;
	GtkWidget *spin_focus;
	GtkWidget *hbox_focus;
	
	GtkAdjustment *adj_eye_sep;
	GtkWidget *spin_eye_sep;
	GtkWidget *hbox_eye_sep;
	
	GtkWidget *vbox_win, *Frame_win;
	GtkWidget *vbox_eye, *Frame_eye;
	GtkWidget *vbox_looking, *Frame_looking;
	GtkWidget *vbox_scale, *Frame_scale;
	GtkWidget *vbox_rotation, *Frame_rotation;
	GtkWidget *vbox_aperture, *Frame_aperture;
	GtkWidget *vbox_streo, *Frame_streo;
	
	GtkWidget *vbox_viewmatrix_save;
	GtkWidget *hbox_viewmatrix_save;
	GtkWidget *entry_viewmatrix_file;
	GtkWidget *saveView_Button, *loadView_Button;
};

/*  prototypes */

void update_windowsize_menu(struct view_widgets *view_menu, GtkWidget *window);
void set_viewmatrix_value(struct view_widgets *view_menu, GtkWidget *window);
void update_viewmatrix_menu(struct view_widgets *view_menu, GtkWidget *window);

GtkWidget * init_viewmatrix_menu_expander(struct view_widgets *view_menu, GtkWidget *window);
#endif
