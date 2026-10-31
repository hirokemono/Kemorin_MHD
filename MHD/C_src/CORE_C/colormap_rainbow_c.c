/*
// colormap_rainbow_c.c 
//   Construct rainbow colormap
*/

#include "colormap_rainbow_c.h"

#include "../GLSL/colormap_rainbow.frag"

void colormap_rainbow_c(double rnorm, double *r, double *g, double *b){
    float x = (float) rnorm;
    *r = (double) colormap_rainbow_red(x);
    *g = (double) colormap_rainbow_green(x);
    *b = (double) colormap_rainbow_blue(x);
	return;
};
