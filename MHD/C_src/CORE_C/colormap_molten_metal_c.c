/*
// colormap_molten_metal_c.c 
//   Construct molten metal colormap
*/

#include "colormap_molten_metal_c.h"

#include "../GLSL/colormap_molten_metal.frag"

void colormap_molten_metal_c(double rnorm, double *r, double *g, double *b){
    float x = (float) rnorm;
    *r = (double) colormap_metal_red(x);
    *g = (double) colormap_metal_green(x);
    *b = 0.0;
	return;
};
