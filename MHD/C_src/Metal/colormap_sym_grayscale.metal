/*
//  colormap_sym_grayscale.metal
//    float4 colormap_sym_grayscale(float x);
*/

#include <metal_stdlib>

float colormap_sym_gray_rgb(float x) {
	if (x < 0.0){
		return 0.0;
	} else if (x > 1.0){
		return 0.0;
	} else if (x >= 0.0 && x < 0.5){
		return 1.0*x;
	} else {
		return 1.0*(1.0 - x);
	}
}

float4 colormap_sym_grayscale(float x) {
    return float4(colormap_sym_gray_rgb(x), colormap_sym_gray_rgb(x), colormap_sym_gray_rgb(x), 1.0);
}
