/*
// colormap_orange_cyan_c.c 
//   Construct colormap from cyan to orange
*/

#include "colormap_orange_cyan_c.h"

#include "../GLSL/colormap_orange_cyan.frag"

void colormap_orange_cyan_c(double rnorm, double *r, double *g, double *b){
    float x = (float) rnorm;
    *r = (double) colormap_orangecyan_red(x);
    *g = (double) colormap_orangecyan_green(x);
    *b = (double) colormap_orangecyan_blue(x);
	return;
};
