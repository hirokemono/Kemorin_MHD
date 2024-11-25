/*
 *  kemoview_gtk_fileselector.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_FILESELECTOR_
#define KEMOVIEW_GTK_FILESELECTOR_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_GTK.h"
#include "kemoviewer.h"

/*  prototypes */
/* Routines for inout from console */

int kemoview_gtk_read_file_select(GtkButton *button, gpointer data);
int kemoview_gtk_save_file_select(GtkButton *button, gpointer data);

struct kv_string * kemoview_read_file_panel(GtkWidget *window_cmap);
struct kv_string * kemoview_save_file_panel(GtkWidget *window_cmap);


void set_pickup_command_gtk(struct kv_string *filename);

#endif /* KEMOVIEW_GTK_FILESELECTOR_ */
