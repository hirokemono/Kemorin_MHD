/*
// set_rgba_table_c.h
*/

#ifndef SET_RGBA_TABLE_C_
#define SET_RGBA_TABLE_C_

#include <stdio.h>
#include <stdlib.h>
#include "m_color_table_c.h"
#include "skip_comment_c.h"
#include "t_ctl_data_pvr_colormap_c.h"
#include "ctl_data_pvr_colormap_IO_c.h"
#include "colormap_rainbow_c.h"
#include "colormap_red_blue_c.h"
#include "colormap_sym_gray_c.h"
#include "colormap_grayscale_c.h"
#include "colormap_orange_cyan_c.h"
#include "colormap_molten_metal_c.h"
#include "colormap_space_c.h"


extern const char * const label_rainbow;
extern const char * const label_bluered;
extern const char * const label_grayscale;
extern const char * const label_sym_gray;
extern const char * const label_orangecyan;
extern const char * const label_molten_metal;
extern const char * const label_space_color;

extern const char * const hd_minmax_c;
extern const char * const hd_linear_c;
extern const char * const hd_nonlinear_c;
extern const char * const hd_colorlist_c;

extern const char * const hd_constant_c;
extern const char * const hd_pointlinear_c;
extern const char * const hd_pointrange_c;
extern const char * const hd_pointdelta_c;
extern const char * const hd_intensity_c;

extern const char * const color_labels[];
extern const char * const mesh_color_list[];


/* prototypes */


void copy_colormap_name_to_ctl(struct colormap_params *cmap_s, 
                               struct chara_ctl_item *colormap_mode);

void cal_rgb_from_value_s(int id_color_mode, double rnorm,
                          double *red, double *green, double *blue);
void set_rgb_from_rgb(struct colormap_params *cmap_s,
                      float red, float green, float blue);
double set_opacity_from_value_s(struct colormap_array *omap_array,
                                double value);

void set_each_color_point_s(struct colormap_params *cmap_s, 
			int i_point, double value, double color);
void set_each_opacity_point_s(struct colormap_params *cmap_s, 
			int i_point, double value, double opacity);

void set_color_mode_by_id(struct colormap_params *cmap_s, int isel);


double get_minimum_opacity_s(struct colormap_params *cmap_s);
double get_maximum_opacity_s(struct colormap_params *cmap_s);
int get_color_mode_id_s(struct colormap_params *cmap_s);
int get_color_table_num_s(struct colormap_params *cmap_s);
int get_opacity_table_num_s(struct colormap_params *cmap_s);

void get_color_table_items_s(struct colormap_params *cmap_s, 
                             int i_point, double *value, double *color);
void get_opacity_table_items_s(struct colormap_params *cmap_s, 
                               int i_point, double *value, double *opacity);

void set_linear_colormap(struct colormap_params *cmap_s, double val_min, double val_max);
void set_constant_opacitymap(struct colormap_params *cmap_s,
                             double val_min, double val_max, float opacity);
void set_full_opacitymap(struct colormap_params *cmap_s, double val_min, double val_max);

void copy_colormap_from_ctl(struct chara_ctl_item *f_colormap_mode_ctl, 
                            struct real2_clist *f_colortbl_ctl, struct colormap_params *cmap_s);
void copy_opacity_from_ctl(struct real2_clist *f_linear_opacity_ctl, 
                           struct colormap_params *cmap_s);

void check_colormap_control_file_s(const int iflag_draw_time, const int iflag_draw_axis,
                                   const int iflag_draw_cbar, struct colormap_params *cmap_s);
void write_colormap_control_file_s(const char *file_name, 
                                   const int iflag_draw_time, const int iflag_draw_axis, 
                                   const int iflag_draw_cbar, struct colormap_params *cmap_s);
void read_colormap_control_file_s(const char *file_name, struct colormap_params *cmap_s);


#endif
