/*
 *  kemoview_glut_console_input.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */
#ifndef KEMOVIEW_GLUT_CONSOLE_INPUT__
#define KEMOVIEW_GLUT_CONSOLE_INPUT__

#include <stdio.h>
#include <string.h>
#include "kemoviewer.h"
#include "kemoview_glut_routines.h"

/*  prototypes */
/* Routines for inout from console */

void input_file_name(char *file_name);
void set_pickup_command(struct kv_string *filename);
void input_file_header(char *file_head);
int input_image_format();
void read_kemoview_data_glut();

/* Routines for values from console input */
void set_psf_range_console();
void set_fline_range_console();
void set_fline_thick_console();
void set_num_isoline();
void set_psf_vector_increment();
void set_psf_vector_scale();
void set_psf_vector_thickness();
void set_psf_opacity();
void set_domain_opacity();
void set_ele_group_opacity();
void set_surf_group_opacity();
void set_domain_distance_console();
void set_coastline_radius_console();
void set_background_color_console();
void set_num_color_loop_console();
void set_node_size_console();
void read_psf_evolution_steps(int *ist_udt, int *ied_udt, int *inc_udt);
int read_psf_rotation_increment();

void set_psf_single_color_console();
void add_psf_colormap_point_console();
void modify_psf_colormap_point_console(int i_point);
void add_psf_opacitymap_point_console();
void modify_psf_opacitymap_point_console(int i_point);

void save_PSF_colormap_file_glut();
void load_PSF_colormap_file_glut();

void save_viewmatrix_file();
void load_viewmatrix_file();

#endif
