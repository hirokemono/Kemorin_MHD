
/* rainbow_color_code_c.h */

#ifndef RAINBOW_COLOR_CODE_C_
#define RAINBOW_COLOR_CODE_C_

#include "kemoviewer_param_c.h"
#include "m_color_table_c.h"
#include "set_rgba_table_c.h"
#include "set_rgb_colors_c.h"
#include "colormap_rainbow_c.h"
#include "colormap_grayscale_c.h"


/* prototypes */
int set_same_id_by_N_c(int inum, int iloop, int i_min, int i_max);
void set_two_color_scale_c(int id_color_mode,
                           double val, double *f_color);
void set_two_color_scale_g(double val, double *f_color);
void set_two_range_cyan_color_c(double val, double *f_color);
void set_rainbow_color_code(struct colormap_array *cmap_array,
                            struct colormap_array *omap_array,
                            int id_color_mode, double val_pe,
							double *f_color);


void set_patch_color_mode_c(int surface_color, int color_mode, int color_loop, 
							int ip, int num_pe, int igrp, int num_grp, double opacity,
                            float single_color[4], double *f_color);
void set_grid_color_mode_c(int line_color, int color_mode, int color_loop, 
						   int ip, int num_pe, int igrp, int num_grp,
                           float single_color[4], double *f_color);
void set_node_color_mode_c(int surface_color, int color_mode, int color_loop,
                           int igrp, int num_grp, float single_color[4]);

void set_black_color_c(double *f_color);
void copy_rgba_color_c(float input_color4[4], float copy_color4[4]);

#endif
