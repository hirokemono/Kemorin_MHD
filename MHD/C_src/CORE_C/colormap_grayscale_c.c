/*
// colormap_grayscale_c.c 
//   Construct grayscale colormap
*/

#include "colormap_grayscale_c.h"


void colormap_grayscale_c(double rnorm, double *r, double *g, double *b){
	if (rnorm < ZERO){
		*r = ZERO;
		*g = ZERO;
		*b = ZERO;
	} else if (rnorm > ONE){
		*r = ONE;
		*g = ONE;
		*b = ONE;
	} else {
		*r = ONE*rnorm;
		*g = ONE*rnorm;
		*b = ONE*rnorm;
	}
	return;
};
