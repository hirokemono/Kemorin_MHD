/*
// colormap_orange_cyan_c.c 
//   Construct colormap from cyan to orange
*/

#include "colormap_orange_cyan_c.h"


void colormap_orange_cyan_c(double rnorm, double *r, double *g, double *b){
    double blue =  0.0;
    double white = 0.5;
    double red =   1.0;
    
    double x = rnorm;
    
    if (x < blue){
        *r = 0.0;
        *g = 1.0;
        *b = 1.0;
    }else if(x < white){
        *r = x * 2.0;
        *g = 1.0;
        *b = 1.0 - x * 0.5;
    }else if(x < red){
        *r = 1.0;
        *g = (red - x) + 0.5;
        *b = (red - x) * 1.5;
    }else{
        *r = 1.0;
        *g = 0.5;
        *b = 0.0;
    };
	return;
};
