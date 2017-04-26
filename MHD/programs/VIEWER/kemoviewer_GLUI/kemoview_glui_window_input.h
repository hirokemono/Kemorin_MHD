/*
 *  kemoview_glui_window_input.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GLUI_WINDOW_INPUT_
#define KEMOVIEW_GLUI_WINDOW_INPUT_

#include <string.h>
#include <GL/glui.h>

#include "kemoviewer.h"
#include "kemoview_glut_routines.h"

/* prototypes */

void set_psf_range_by_glui(int winid);
void set_fline_range_by_glui(int winid);
void set_fline_thick_glui(int winid);

void set_psf_opacity_by_glui(int winid);
void set_domain_opacity_by_glui(int winid);
void set_ele_group_opacity_by_glui(int winid);
void set_surf_group_opacity_by_glui(int winid);

void set_num_isoline_from_glui(int winid);
void set_psf_vect_increment_glui(int winid);
void set_psf_vector_scale_by_glui(int winid);
void set_psf_vector_thick_by_glui(int winid);
void set_coastline_radius_glui(int winid);
void set_domain_distance_by_glui(int winid);
void set_num_color_loop_by_glui(int winid);
void set_node_size_by_glui(int winid);

void edit_psf_colormap_by_glui(int winid);
void edit_psf_opacitymap_by_glui(int winid);


#endif
