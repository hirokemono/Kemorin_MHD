/*
// colormap_red_blue_c.c 
//   Construct colormap from blue to red
*/

#include "colormap_red_blue_c.h"

#include "../GLSL/colormap_red_blue.frag"

void colormap_red_blue_c(double rnorm, double *r, double *g, double *b){
    float x = (float) rnorm;
    *r = (double) colormap_redblue_red(x);
    *g = (double) colormap_redblue_green(x);
    *b = (double) colormap_redblue_blue(x);
	return;
};
