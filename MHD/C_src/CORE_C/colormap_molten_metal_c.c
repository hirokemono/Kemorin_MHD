/*
// colormap_molten_metal_c.c 
//   Construct molten metal colormap
*/

#include "colormap_molten_metal_c.h"


void colormap_molten_metal_c(double rnorm, double *r, double *g, double *b){
    double c_g1 = 0.6;
    double r_mul = ONE / c_g1;
    double g_mul = ONE / (ONE - c_g1);

    double x = rnorm;
    
    if (x < ZERO){
        *r = 0.0;
    }else if (x < c_g1){
        *r = x * r_mul;
    }else{
        *r = 1.0;
    };
    
    if (x < c_g1){
        *g = 0.0;
    }else if(x < 1.0){
        *g = (x - c_g1) * g_mul;
    }else{
        *g = 1.0;
    };
    
    *b = 0.0;
	return;
};
