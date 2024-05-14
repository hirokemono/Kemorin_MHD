/*
// colormap_rainbow_c.c 
//   Construct rainbow colormap
*/

#include "colormap_rainbow_c.h"


void colormap_rainbow_c(double rnorm, double *r, double *g, double *b){
	double purple = ZERO;
	double blue =   DECI;
	double ocean =  0.325;
	double green =  0.55;
	double yellow = 0.775;
	double red =    ONE;
			
	if (rnorm < purple){
		*r = HALF;
		*g = ZERO;
		*b = ONE;
	} else if (rnorm >= purple && rnorm < blue){
		*r = HALF - FIVE*rnorm;
		*g = ZERO;
		*b = ONE;
	} else if (rnorm >= blue && rnorm < ocean){
		*r = ZERO;
		*g = FOURTY*(rnorm-blue) / NINE;
		*b = ONE;
	} else if (rnorm >= ocean && rnorm < green){
		*r = ZERO;
		*g = ONE;
		*b = ONE - FOURTY*(rnorm-ocean) / NINE;
	} else if (rnorm >= green && rnorm < yellow){
		*r = FOURTY*(rnorm-green) / NINE;
		*g = ONE;
		*b = ZERO;
	} else if (rnorm >= yellow && rnorm < red){
		*r = ONE;
		*g = ONE - FOURTY*(rnorm-yellow) / NINE;
		*b = ZERO;
	} else {
		*r = ONE;
		*g = ZERO;
		*b = ZERO;
	}
	return;
};
