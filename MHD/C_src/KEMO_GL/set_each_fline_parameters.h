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
#include "m_psf_data_4_viewer_c.h"
#include "m_kemoview_fline_menu.h"
#include "skip_comment_c.h"
#include "kemoviewer_base.h"
#include "set_rgba_table_c.h"
#include "set_texture_4_psf.h"
#include "draw_patches_4_PSF.h"
#include "read_data_4_kemoviewer.h"

/* prototypes */


void get_fline_full_path_file_name(struct fline_menu_val *fline_m, 
			struct kv_string *ucd_m);
int get_fline_file_step_prefix(struct fline_menu_val *fline_m, 
			struct kv_string *fline_filehead);
void set_fline_file_step(struct fline_menu_val *fline_m, int istep);

void set_fline_switch(struct fline_menu_val *fline_m, int iflag);
void set_fline_color_type(struct fline_menu_val *fline_m, int iflag);

int get_fline_switch(struct fline_menu_val *fline_m);
int get_fline_color_num_field(struct psf_data *fline_d);
int get_fline_color_ncomptot(struct psf_data *fline_d);
int fline_color_num_comps(struct psf_data *fline_d, int i);
int get_fline_color_istack(struct psf_data *fline_d, int i);
void get_fline_color_data_name(struct psf_data *fline_d, 
			struct kv_string *colorname, int i);
int get_fline_color_field(struct fline_menu_val *fline_m);
int get_fline_color_component(struct fline_menu_val *fline_m);
int get_fline_color_data_adress(struct fline_menu_val *fline_m);
int get_fline_colormode(struct fline_menu_val *fline_m);


void set_fline_type(struct fline_menu_val *fline_m, int iflag);
int get_fline_type(struct fline_menu_val *fline_m);
int toggle_fline_type(struct fline_menu_val *fline_m);

void set_fline_thickness(double value, struct fline_menu_val *fline_m);
double get_fline_thickness(struct fline_menu_val *fline_m);

double get_fline_data_min(struct psf_data *fline_d, int i);
double get_fline_data_max(struct psf_data *fline_d, int i);


void set_fline_linear_colormap(double minvalue, int i_min_digit, double maxvalue, int i_max_digit, 
							   struct fline_menu_val *fline_m);
void set_fline_constant_opacity(struct psf_data *fline_d, struct fline_menu_val *fline_m,
			double opacity);

double get_fline_opacity_at_value(struct fline_menu_val *fline_m, double value);
void set_fline_color_data(struct fline_menu_val *fline_m, 
			int i_point, double value, double color);
void set_fline_opacity_data(struct fline_menu_val *fline_m, 
			int i_point, double value, double opacity);
void set_fline_color_mode_id(struct fline_menu_val *fline_m, int isel);

double get_fline_min_color(struct fline_menu_val *fline_m);
double get_fline_max_color(struct fline_menu_val *fline_m);
double get_fline_min_opacity(struct fline_menu_val *fline_m);
double get_fline_max_opacity(struct fline_menu_val *fline_m);

int get_fline_color_num(struct fline_menu_val *fline_m);
int get_fline_opacity_num(struct fline_menu_val *fline_m);


void get_fline_color_item(struct fline_menu_val *fline_m,
			int i_point, double *value, double *color);
void get_fline_opacity_item(struct fline_menu_val *fline_m,
			int i_point, double *value, double *opacity);
void write_fline_colormap_file(struct kv_string *filename, 
                               const int iflag_draw_axis, struct fline_menu_val *fline_m);
void read_fline_colormap_file(struct kv_string *filename, struct fline_menu_val *fline_m);

#endif
