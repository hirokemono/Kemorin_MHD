/*
//  colormap_rainbow.metal
//    float4 colormap_rainbow(float x, float alpha);
*/

#include <metal_stdlib>


float colormap_rainbow_blue(float x) {
    float ocean =  0.325;
    float green =  0.55;
        
    float c;
	if (x < ocean){
        c = 1.0;
	} else if (x >= ocean && x < green){
        c = 1.0 - 40.0*(x-ocean) / 9.0;
	} else {
        c = 0.0;
	}
    return c;
}

float colormap_rainbow_green(float x) {
	float blue =   0.1;
    float ocean =  0.325;
    float yellow = 0.775;
    float red =    1.0;
			
    float c;
	if (x < blue){
        c = 0.0;
	} else if (x >= blue && x < ocean){
        c = 40.0*(x-blue) / 9.0;
	} else if (x >= ocean && x < yellow){
        c = 1.0;
	} else if (x >= yellow && x < red){
        c = 1.0 - 40.0*(x-yellow) / 9.0;
	} else {
        c = 0.0;
	}
    return c;
}

float colormap_rainbow_red(float x) {
    float purple = 0.0;
    float blue =   0.1;
    float green =  0.55;
    float yellow = 0.775;
			
    float c;
	if (x < purple){
        c = 0.5;
	} else if (x >= purple && x < blue){
        c = 0.5 - 5.0*x;
	} else if (x >= blue && x < green){
        c = 0.0;
	} else if (x >= green && x < yellow){
        c = 40.0*(x-green) / 9.0;
	} else {
        c = 1.0;
	}
    return c;
}

float4 colormap_rainbow(float x, float alpha) {
    return float4(colormap_rainbow_red(x), colormap_rainbow_green(x), colormap_rainbow_blue(x), alpha);
}
