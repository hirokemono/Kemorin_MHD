/*
 *  kemoview_gtk_window_input.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GTK_WINDOW_INPUT__
#define KEMOVIEW_GTK_WINDOW_INPUT__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gtk/gtk.h"

#include "kemoviewer.h"
#include "kemoview_glut_routines.h"
#include "tree_view_4_pvr_colormap.h"
#include "tree_view_4_light_position.h"
#include "kemoview_gtk_colormap_menu.h"

/*  prototypes */

void set_psf_single_color_gtk();
void edit_psf_colormap_gtk(struct kemoviewer_type *single_kemoview);
void set_psf_range_gtk();
void set_fline_range_gtk();
void set_fline_thick_gtk();
void set_num_isoline_gtk();
void set_psf_vector_increment_gtk();
void set_psf_vector_scale_gtk();
void set_psf_vector_thickness_gtk();
void set_psf_opacity_gtk();
void set_domain_opacity_gtk();
void set_ele_group_opacity_gtk();
void set_surf_group_opacity_gtk();
void set_coastline_radius_gtk();
void set_background_color_gtk();
void set_domain_distance_gtk();
void set_num_color_loop_gtk();
void set_node_size_gtk();

#endif
