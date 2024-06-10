/*
//  set_each_fline_parameters.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/

#ifndef SET_EACH_FLINE_PARAMETERS_
#define SET_EACH_FLINE_PARAMETERS_

#include <stdio.h>

#include "kemoviewer.h"
#include "m_fline_data_4_viewer_c.h"
#include "m_kemoview_fline_menu.h"
#include "skip_comment_c.h"
#include "kemoviewer_base.h"
#include "set_rgba_table_c.h"
#include "set_texture_4_psf.h"
#include "draw_patches_4_PSF.h"
#include "read_data_4_kemoviewer.h"

/* prototypes */


void get_fline_full_path_file_name(struct psf_menu_val *fline_m,
			struct kv_string *ucd_m);
int get_fline_file_step_prefix(struct psf_menu_val *fline_m,
			struct kv_string *fline_filehead);
void set_fline_file_step(struct psf_menu_val *fline_m, int istep);

void set_fline_switch(struct psf_menu_val *fline_m, int iflag);
void set_fline_color_type(struct psf_menu_val *fline_m, int iflag);

int get_fline_switch(struct psf_menu_val *fline_m);
long get_fline_color_num_field(struct fline_data *fline_d);
long get_fline_color_ncomptot(struct fline_data *fline_d);
long fline_color_num_comps(struct fline_data *fline_d, int i);
long get_fline_color_istack(struct fline_data *fline_d, int i);
void get_fline_color_data_name(struct fline_data *fline_d,
			struct kv_string *colorname, int i);
long get_fline_color_field(struct psf_menu_val *fline_m);
long get_fline_color_component(struct psf_menu_val *fline_m);
long get_fline_color_data_adress(struct psf_menu_val *fline_m);
int get_fline_colormode(struct psf_menu_val *fline_m);


void set_fline_type(struct psf_menu_val *fline_m, long iflag);
long get_fline_type(struct psf_menu_val *fline_m);

void set_fline_thickness(double value, struct psf_menu_val *fline_m);
double get_fline_thickness(struct psf_menu_val *fline_m);

double get_fline_data_min(struct fline_data *fline_d, int i);
double get_fline_data_max(struct fline_data *fline_d, int i);

int send_coordinate_id_fline(struct fline_data *fline_d, struct psf_menu_val *fline_m);

void set_fline_linear_colormap(double minvalue, int i_min_digit, double maxvalue, int i_max_digit, 
							   struct psf_menu_val *fline_m);
void set_fline_constant_opacity(struct fline_data *fline_d,
                                struct psf_menu_val *fline_m,
                                double opacity);

double get_fline_opacity_at_value(struct psf_menu_val *fline_m, double value);
void set_fline_color_data(struct psf_menu_val *fline_m,
			int i_point, double value, double color);
void set_fline_opacity_data(struct psf_menu_val *fline_m,
			int i_point, double value, double opacity);
void set_fline_color_mode_id(struct psf_menu_val *fline_m, int isel);
#endif
