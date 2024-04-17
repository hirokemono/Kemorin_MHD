/*
// colormap_molten_metal_c.c 
//   Construct molten metal colormap
*/

#include "colormap_molten_metal_c.h"


void colormap_molten_metal_c(double rnorm, double *r, double *g, double *b){
    double c_r1 = 0.57147;
    double c_g1 = 0.6;
    
    double x = rnorm;
    
    if (x < ZERO){
        *r = 0.0;
    }else if (x < c_r1){
        *r = x * 1.749873134197771;
    }else{
        *r = 1.0;
    };
    
    if (x < c_g1){
        *g = 0.0;
    }else if(x < 1.0){
        *g = (x - c_g1) / (ONE - c_g1);
    }else{
        *g = 1.0;
    };
    
    *b = 0.0;
	return;
};
