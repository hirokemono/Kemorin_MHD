/*
// colormap_sym_gray_c.c 
//   Construct symmetric grayscale colormap
*/

#include "colormap_sym_gray_c.h"


void colormap_sym_grayscale_c(double rnorm, double *r, double *g, double *b){
	if (rnorm < ZERO){
		*r = ZERO;
		*g = ZERO;
		*b = ZERO;
	} else if (rnorm > ONE){
		*r = ZERO;
		*g = ZERO;
		*b = ZERO;
	} else if (rnorm >= ZERO && rnorm < HALF){
		*r = ONE*rnorm;
		*g = ONE*rnorm;
		*b = ONE*rnorm;
	} else {
		*r = ONE*(ONE - rnorm);
		*g = ONE*(ONE - rnorm);
		*b = ONE*(ONE - rnorm);
	}
	return;
};
