//
//  set_each_psf_parameters.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
//

#ifndef SET_EACH_PSF_PARAMETERS_
#define SET_EACH_PSF_PARAMETERS_

#include <stdio.h>

#include "m_psf_data_4_viewer_c.h"
#include "m_kemoviewer_menu.h"
#include "skip_comment_c.h"
#include "set_rgba_table_c.h"
#include "set_texture_4_psf.h"

/* prototypes */

int send_each_psf_file_header_full(struct psf_menu_val *psf_menu, char *file_head, int *iflag);
int send_each_psf_file_header(struct psf_menu_val *psf_menu, char *file_head);


int send_nfield_each_psf(struct psf_data *psf_d);
int send_ncomptot_each_psf(struct psf_data *psf_d);
int send_ncomp_each_psf(struct psf_data *psf_d, int i);
int send_istack_each_comp_psf(struct psf_data *psf_d, int i);
void send_each_psf_data_name(struct psf_data *psf_d, char *name, int i);


int send_field_draw_each_psf(struct psf_menu_val *psf_menu);
int send_draw_comp_id_psf(struct psf_menu_val *psf_menu);
int send_draw_component_psf(struct psf_menu_val *psf_menu);
int send_coordinate_id_psf(struct psf_data *psf_d, struct psf_menu_val *psf_menu);

void set_texture_psf_glut(struct psf_menu_val *psf_menu, int img_fmt, const char *img_head);

void set_psf_polygon_mode(struct psf_menu_val *psf_menu, int iflag);
int send_each_psf_polygon_mode(struct psf_menu_val *psf_menu);
int toggle_each_psf_polygon_mode(struct psf_menu_val *psf_menu);

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

void set_psf_patch_color(struct psf_menu_val *psf_menu, int iflag);

void set_each_isoline_color(struct psf_menu_val *psf_menu, int iflag);
void set_each_n_isoline(struct psf_menu_val *psf_menu, int nlline);
void set_each_vector_patch_color(struct psf_menu_val *psf_menu, int iflag);
void set_each_increment_vect(struct psf_menu_val *psf_menu, int increment);
void set_each_scale_vect(struct psf_menu_val *psf_menu, double scale);
void set_each_vector_thick(struct psf_menu_val *psf_menu, double size);

int send_each_psf_patch_color(struct psf_menu_val *psf_menu);
int send_each_isoline_color(struct psf_menu_val *psf_menu);
int send_num_isoline(struct psf_menu_val *psf_menu);
int send_each_vector_patch_color(struct psf_menu_val *psf_menu);
int send_increment_vector(struct psf_menu_val *psf_menu);
double send_scale_vector(struct psf_menu_val *psf_menu);
double send_vector_thick(struct psf_menu_val *psf_menu);

double send_psf_data_min(struct psf_data *psf_d, int icomp);
double send_psf_data_max(struct psf_data *psf_d, int icomp);

void realloc_PSF_color_index_list(struct psf_menu_val *psf_menu, int id_cmode, int num);
void realloc_PSF_opacity_index_list(struct psf_menu_val *psf_menu, int num);

void set_PSF_linear_colormap(struct psf_menu_val *psf_menu, double minvalue, double maxvalue);

void set_PSF_constant_opacity(struct psf_data *psf_d, struct psf_menu_val *psf_menu,
                                 double opacity);

void set_PSF_rgb_from_value(struct psf_menu_val *psf_menu,
                            double value, double *red, double *green, double *blue);
void set_PSF_opacity_from_value(struct psf_menu_val *psf_menu, double value, double *opacity);
void set_each_PSF_color_point(struct psf_menu_val *psf_menu, int i_point, double value, double color);
void set_each_PSF_opacity_point(struct psf_menu_val *psf_menu, int i_point, double value, double opacity);
void set_PSF_color_mode_id(struct psf_menu_val *psf_menu, int isel);

double send_each_PSF_color_table_min(struct psf_menu_val *psf_menu);
double send_each_PSF_color_table_max(struct psf_menu_val *psf_menu);
double send_each_PSF_minimum_opacity(struct psf_menu_val *psf_menu);
double send_each_PSF_maximum_opacity(struct psf_menu_val *psf_menu);
int send_each_PSF_color_table_num(struct psf_menu_val *psf_menu);
int send_each_PSF_opacity_table_num(struct psf_menu_val *psf_menu);

void send_each_PSF_color_table_items(struct psf_menu_val *psf_menu, int i_point, double *value, double *color);
void send_each_PSF_opacity_table_items(struct psf_menu_val *psf_menu, int i_point, double *value, double *opacity);

void write_each_PSF_colormap_control_file(struct psf_menu_val *psf_menu, const char *file_name);

#endif
