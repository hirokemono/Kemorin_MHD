
/* set_rgb_colors_c.h */

#ifndef SET_RGB_COLORS_C_
#define SET_RGB_COLORS_C_

#include "kemosrc_param_c.h"
#include "t_control_real2_IO.h"

/*  prototypes */

double color_normalize_linear_c(double d_min, double d_max, double value);
double color_normalize_linear_segment_c(struct real2_clist *colormap_clist, double value);

void color_grayscale_c(double rnorm, double *r, double *g, double *b);
void color_sym_grayscale_c(double rnorm, double *r, double *g, double *b);
void color_redblue_c(double rnorm, double *r, double *g, double *b);
void color_rainbow_c(double rnorm, double *r, double *g, double *b);

#endif
