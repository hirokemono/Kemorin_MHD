/*
 *  kemoview_fileselector_gtk.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_FILESELECTOR_GTK__
#define KEMOVIEW_FILESELECTOR_GTK__

#include <stdio.h>
#include <stdlib.h>

#include "gtk/gtk.h"

#include <string.h>
#include "kemoviewer.h"
#include "kemoview_glut_routines.h"

/*  prototypes */
/* Routines for inout from console */

void kemoview_gtk_read_file_select(gpointer data);
void kemoview_gtk_save_file_select(gpointer data);

struct kv_string * kemoview_read_file_panel(GtkWidget *window_cmap);
struct kv_string * kemoview_save_file_panel(GtkWidget *window_cmap);


void read_kemoview_data_gtk();
int input_texture_file_gtk(struct kv_string *file_prefix);
int output_image_file_gtk(struct kv_string *file_prefix);
int output_evolution_file_gtk(struct kv_string *file_prefix,
			int *ist_udt, int *ied_udt, int *inc_udt);
int output_rotation_file_gtk(struct kv_string *file_prefix,
			int *axis_ID, int *inc_rot);

void save_viewmatrix_file_gtk();
void load_viewmatrix_file_gtk();

#endif
