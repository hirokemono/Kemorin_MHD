/*
// colormap_space_c.c 
//   Construct space colormap
*/

#include "colormap_space_c.h"


void colormap_space_c(double rnorm, double *r, double *g, double *b){
    double c_r1 = 37067.0 / 158860.0;
    double c_r2 = 85181.0 / 230350.0;
    double c_r3 = (sqrt(3196965649.0) + 83129.0) / 310480.0;
    double c_r4 = 231408.0 / 362695.0;
    double c_r5 = 152073.0 / 222340.0;
    double c_r6 = 294791.0 / 397780.0;
    double c_r7 = 491189.0 / 550980.0;
    
    double c_g1 = (-sqrt(166317494.0) + 39104.0) / 183830.0;
    double c_g3 = (3.0 * sqrt(220297369.0) + 58535.0) / 155240.0;
    
    double c_b1 = 51987.0 / 349730.0;
    
    double xx;
    double x = rnorm;
    
    if (x < c_r1){
        *r = 0.0;
    }else if(x < c_r2){
        xx = x - c_r1;
        *r = (780.25 * xx + 319.71) * xx / 255.0;
    }else if(x < c_r3){
        *r = ((1035.33580904442 * x - 82.5380748768798) * x
             - 52.8985266363332) / 255.0;
    }else if (x < c_r4){
        *r = (339.41 * x - 33.194) / 255.0;
    }else if (x < c_r5){
            *r = (1064.8 * x - 496.01) / 255.0;
    }else if (x < c_r6){
        *r = (397.78 * x - 39.791) / 255.0;
    }else if (x < c_r7){
        *r = 1.0;
    }else if (x < ONE){
        *r = (5509.8 * x + 597.91) * x / 255.0;
    }else{
        *r = 1.0;
    };

    if (x < ZERO){
        *g = 0.0;
    }else if (x < c_g1){
        *g = (-1838.3 * x + 464.36) * x / 255.0;
    }else if (x < c_r1){
        *g = (-317.72 * x + 74.134) / 255.0;
    }else if (x < c_g3){
        *g = 0.0;
    }else if (x < c_r6){
        xx = x - c_g3;
        *g = (-1945.0 * xx + 1430.2) * xx / 255.0;
    }else if (x < c_r7){
        *g = ((-1770.0 * x + 3.92813840044638e3) * x
             - 1.84017494792245e3) / 255.0;
    }else{
        *g = 1.0;
    };
    
    if (x < ZERO){
        *b = 0.0;
    }else if (x < c_b1){
        *b = (458.79 * x) / 255.0;
    }else if (x < c_r2){
        *b = (109.06 * x + 51.987) / 255.0;
    }else if (x < c_r3){
        *b = (339.41 * x - 33.194) / 255.0;
    }else if (x < c_g3){
        *b = ((-1552.4 * x + 1170.7) * x - 92.996) / 255.0;
    }else if (x < 27568.0 / 38629.0){
        *b = 0.0;
    }else if (x < 81692.0 / 96241.0){
        *b = (386.29 * x - 275.68) / 255.0;
    }else if (x < 1.0){
        *b = (1348.7 * x - 1092.6) / 255.0;
    }else{
        *b = 1.0;
    };
	return;
};
