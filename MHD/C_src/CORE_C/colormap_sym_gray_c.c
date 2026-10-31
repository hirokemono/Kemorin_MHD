/*
// colormap_sym_gray_c.c 
//   Construct symmetric grayscale colormap
*/

#include "colormap_sym_gray_c.h"

#include "../GLSL/colormap_sym_grayscale.frag"


void colormap_sym_grayscale_c(double rnorm, double *r, double *g, double *b){
    float x = (float) rnorm;
    *r = (double) colormap_sym_gray_rgb(x);
    *g = *r;
    *b = *r;
	return;
};
