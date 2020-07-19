/*
//  set_each_psf_parameters.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/

#ifndef SET_EACH_PSF_PARAMETERS_
#define SET_EACH_PSF_PARAMETERS_

#include <stdio.h>

#include "kemoviewer.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_kemoview_psf_menu.h"
#include "skip_comment_c.h"
#include "kemoviewer_base.h"
#include "set_rgba_table_c.h"
#include "set_texture_4_psf.h"
#include "draw_patches_4_PSF.h"
#include "numbers_to_bin_c.h"

/* prototypes */

int send_each_psf_file_header_full(struct psf_menu_val *psf_menu, 
			struct kv_string *psf_filehead, int *iflag);
int send_each_psf_file_dir_head(struct psf_menu_val *psf_menu
			, struct kv_string *stripped_dir, struct kv_string *stripped_filehead);


int send_nfield_each_psf(struct psf_data *psf_d);
int send_ncomptot_each_psf(struct psf_data *psf_d);
int send_ncomp_each_psf(struct psf_data *psf_d, int i);
int send_istack_each_comp_psf(struct psf_data *psf_d, int i);
void send_each_psf_data_name(struct psf_data *psf_d, struct kv_string *colorname, int i);


int send_field_draw_each_psf(struct psf_menu_val *psf_menu);
int send_draw_comp_id_psf(struct psf_menu_val *psf_menu);
int send_draw_component_psf(struct psf_menu_val *psf_menu);
int send_coordinate_id_psf(struct psf_data *psf_d, struct psf_menu_val *psf_menu);

void set_texture_psf_from_bgra(struct psf_menu_val *psf_menu,
			int width, int height, const unsigned char *bgra_in);

void set_psf_polygon_mode(struct psf_menu_val *psf_menu, int iflag);
int send_each_psf_polygon_mode(struct psf_menu_val *psf_menu);
int toggle_each_psf_polygon_mode(struct psf_menu_val *psf_menu);

void set_psf_vector_mode(struct psf_menu_val *psf_menu, int iflag);
int send_each_psf_vector_mode(struct psf_menu_val *psf_menu);
int toggle_each_psf_vector_mode(struct psf_menu_val *psf_menu);

int send_draw_psf_solid(struct psf_menu_val *psf_menu);
int toggle_draw_psf_solid(struct psf_menu_val *psf_menu);

int send_draw_psf_grid(struct psf_menu_val *psf_menu);
int toggle_draw_psf_grid(struct psf_menu_val *psf_menu);

int send_draw_psf_zero(struct psf_menu_val *psf_menu);
int toggle_draw_psf_zero(struct psf_menu_val *psf_menu);

int send_draw_psf_cbar(struct psf_menu_val *psf_menu);
int toggle_draw_psf_cbar(struct psf_menu_val *psf_menu);

int send_draw_psf_vect(struct psf_menu_val *psf_menu);
int toggle_draw_psf_vect(struct psf_menu_val *psf_menu);

int send_draw_psf_refv(struct psf_menu_val *psf_menu);
int toggle_draw_psf_refv(struct psf_menu_val *psf_menu);

void set_psf_patch_color_mode(struct psf_menu_val *psf_menu, int iflag);

void set_each_isoline_color(struct psf_menu_val *psf_menu, int iflag);
void set_each_n_isoline(struct psf_menu_val *psf_menu, int nlline);
void set_each_isoline_width(double value, struct psf_menu_val *psf_menu);
void set_each_vector_patch_color(struct psf_menu_val *psf_menu, int iflag);
void set_each_increment_vect(int increment, struct psf_menu_val *psf_menu);
void set_each_scale_vect(double value, struct psf_menu_val *psf_menu);
void set_each_vector_thick(double value, struct psf_menu_val *psf_menu);

int send_each_psf_patch_color(struct psf_menu_val *psf_menu);
int send_each_isoline_color(struct psf_menu_val *psf_menu);
int send_num_isoline(struct psf_menu_val *psf_menu);
double send_isoline_width(struct psf_menu_val *psf_menu);
int send_each_vector_patch_color(struct psf_menu_val *psf_menu);

int send_each_increment_vect(struct psf_menu_val *psf_menu);
double send_scale_vector(struct psf_menu_val *psf_menu);
double send_vector_thick(struct psf_menu_val *psf_menu);

double send_psf_data_min(struct psf_data *psf_d, int icomp);
double send_psf_data_max(struct psf_data *psf_d, int icomp);

void delete_PSF_color_index_list(struct psf_menu_val *psf_menu, int i_delete);
void delete_PSF_opacity_index_list(struct psf_menu_val *psf_menu, int i_delete);

void add_PSF_color_index_list(struct psf_menu_val *psf_menu, double add_value, double add_color);
void add_PSF_opacity_index_list(struct psf_menu_val *psf_menu, double add_value, double add_opacity);

void set_PSF_linear_colormap(double minvalue, int i_min_digit, double maxvalue, int i_max_digit,
							 struct psf_menu_val *psf_menu);

void set_PSF_fixed_color(struct psf_data *psf_d, struct psf_menu_val *psf_menu,
                         double *rgba);
void set_PSF_constant_opacity(struct psf_data *psf_d, struct psf_menu_val *psf_menu,
                                 double opacity);

void set_PSF_rgb_from_value(struct psf_menu_val *psf_menu,
                            double value, double *red, double *green, double *blue);
double get_PSF_opacity_at_value(struct psf_menu_val *psf_menu, double value);
void set_each_PSF_color_point(struct psf_menu_val *psf_menu, int i_point, double value, double color);
void set_each_PSF_opacity_point(struct psf_menu_val *psf_menu, int i_point, double value, double opacity);
void set_PSF_colormap_id(struct psf_menu_val *psf_menu, int isel);

int send_PSF_colormap_id(struct psf_menu_val *psf_menu);
double send_each_PSF_color_table_min(struct psf_menu_val *psf_menu);
double send_each_PSF_color_table_max(struct psf_menu_val *psf_menu);
double send_each_PSF_minimum_opacity(struct psf_menu_val *psf_menu);
double send_each_PSF_maximum_opacity(struct psf_menu_val *psf_menu);
int send_each_PSF_color_table_num(struct psf_menu_val *psf_menu);
int send_each_PSF_opacity_table_num(struct psf_menu_val *psf_menu);

void send_each_PSF_color_table_items(struct psf_menu_val *psf_menu, int i_point, double *value, double *color);
void send_each_PSF_opacity_table_items(struct psf_menu_val *psf_menu, int i_point, double *value, double *opacity);

void write_each_PSF_colormap_control_file(const char *file_name, const int iflag_draw_axis, 
                                          struct psf_menu_val *psf_menu);
void read_each_PSF_colormap_control_file(struct psf_menu_val *psf_menu, const char *file_name);
void check_each_PSF_colormap_control(int iflag_draw_axis, struct psf_menu_val *psf_menu);

#endif
