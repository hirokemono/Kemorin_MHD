/*
// colormap_space_c.c 
//   Construct space colormap
*/

#include "colormap_space_c.h"

#include "../GLSL/colormap_space.frag"

void colormap_space_c(double rnorm, double *r, double *g, double *b){
    float x = (float) rnorm;
    *r = (double) colormap_space_red(x);
    *g = (double) colormap_space_green(x);
    *b = (double) colormap_space_blue(x);
	return;
};
