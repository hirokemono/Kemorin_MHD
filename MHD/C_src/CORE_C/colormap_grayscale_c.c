/*
// colormap_grayscale_c.c 
//   Construct grayscale colormap
*/

#include "colormap_grayscale_c.h"

#include "../GLSL/colormap_grayscale.frag"

void colormap_grayscale_c(double rnorm, double *r, double *g, double *b){
    float x = (float) rnorm;
    *r = (double) colormap_gray_rgb(x);
    *g = *r;
    *b = *r;
	return;
};
