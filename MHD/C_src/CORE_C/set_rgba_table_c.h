
/* set_rgba_table_c.h */

#ifndef SET_RGBA_TABLE_C_
#define SET_RGBA_TABLE_C_

#include <stdio.h>
#include <stdlib.h>
#include "m_color_table_c.h"


/* prototypes */


void set_rgb_from_value_s(struct colormap_params *cmap_s,
			double value, double *red, double *green, double *blue);
void set_rgb_from_rgb(struct colormap_params *cmap_s,
                      double red, double green, double blue);
void set_opacity_from_value_s(struct colormap_params *cmap_s, 
			double value, double *opacity);

void set_each_color_point_s(struct colormap_params *cmap_s, 
			int i_point, double value, double color);
void set_each_opacity_point_s(struct colormap_params *cmap_s, 
			int i_point, double value, double opacity);

void set_color_mode_id_s(struct colormap_params *cmap_s, int isel);


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
			double val_min, double val_max, double opaciy);
void set_full_opacitymap(struct colormap_params *cmap_s, double val_min, double val_max);

void output_colormap_control_s(FILE *fp, struct colormap_params *cmap_s);
void write_colormap_control_file_s(const char *file_name, struct colormap_params *cmap_s);


#endif
