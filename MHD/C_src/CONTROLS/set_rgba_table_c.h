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


extern const char color_labels[4][KCHARA_C];

/* prototypes */


void copy_colormap_name_to_ctl(struct colormap_params *cmap_s, 
			struct chara_ctl_item *colormap_mode);

void set_rgb_from_value_s(struct colormap_params *cmap_s,
			double value, double *red, double *green, double *blue);
void set_rgb_from_rgb(struct colormap_params *cmap_s,
			double red, double green, double blue);
double set_opacity_from_value_s(struct colormap_params *cmap_s, 
			double value);

void set_each_color_point_s(struct colormap_params *cmap_s, 
			int i_point, double value, double color);
void set_each_opacity_point_s(struct colormap_params *cmap_s, 
			int i_point, double value, double opacity);

void set_color_mode_by_id(struct colormap_params *cmap_s, int isel);


double send_minimum_opacity_s(struct colormap_params *cmap_s);
double send_maximum_opacity_s(struct colormap_params *cmap_s);
int send_color_mode_id_s(struct colormap_params *cmap_s);
int send_color_table_num_s(struct colormap_params *cmap_s);
int send_opacity_table_num_s(struct colormap_params *cmap_s);

void send_color_table_items_s(struct colormap_params *cmap_s, 
			int i_point, double *value, double *color);
void send_opacity_table_items_s(struct colormap_params *cmap_s, 
			int i_point, double *value, double *opacity);

void set_linear_colormap(struct colormap_params *cmap_s, double val_min, double val_max);
void set_constant_opacitymap(struct colormap_params *cmap_s,
			double val_min, double val_max, double opacity);
void set_full_opacitymap(struct colormap_params *cmap_s, double val_min, double val_max);

void copy_colormap_from_ctl(struct chara_ctl_item *colormap_mode_ctl, 
			struct real2_clist *colortbl_list, struct colormap_params *cmap_s);
void copy_opacity_from_ctl(struct real2_clist *linear_opacity_list, 
			struct colormap_params *cmap_s);

void check_colormap_control_file_s(const int iflag_draw_axis, const int draw_psf_cbar, 
                                   struct colormap_params *cmap_s);
void write_colormap_control_file_s(const char *file_name, const int iflag_draw_axis, 
                                   const int draw_psf_cbar, struct colormap_params *cmap_s);
void read_colormap_control_file_s(const char *file_name, struct colormap_params *cmap_s);


#endif
