/*
// colormap_red_blue_c.c 
//   Construct colormap from blue to red
*/

#include "colormap_red_blue_c.h"


void colormap_red_blue_c(double rnorm, double *r, double *g, double *b){
	double abyss =   ZERO;
	double blue =     0.1;
	double white =  ONE / TWO;
	double red =      0.9;
	double blood =    ONE;
	
	if (rnorm < abyss){
		*r = ZERO;
		*g = 0.2;
		*b = 0.8;
	} else if (rnorm >= abyss && rnorm < blue){
		*r = ZERO;
		*g = 2.0 * (blue - rnorm);
		*b = 0.8 + 2.0 * rnorm;
	} else if (rnorm >= blue && rnorm < white){
		*r = (rnorm - blue) * 2.0;
		*g = (rnorm - blue) * 2.0;
		*b = ONE - (rnorm - blue) * 0.25;
	} else if (rnorm >= white && rnorm < red){
		*r = ONE - (red - rnorm) * 0.25;
		*g = (red - rnorm) * 2.0;
		*b = (red - rnorm) * 2.0;
	} else if (rnorm >= red && rnorm < blood){
		*r = ONE - (rnorm - red) * 2.0;
		*g = ZERO;
		*b = ZERO;
	} else {
		*r = 0.8;
		*g = ZERO;
		*b = ZERO;
	}
	return;
};
