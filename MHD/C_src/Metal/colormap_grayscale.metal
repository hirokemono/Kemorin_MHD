/*
//  colormap_grayscale.metal
//    float4 colormap_grayscale(float x, float alpha);
*/

#include <metal_stdlib>

float colormap_gray_rgb(float x) {
    float c = 0.0;
	if (x < 0.0){
        c = 0.0;
	} else if (x > 1.0){
        c = 1.0;
	} else {
        c = 1.0*x;
	}
    return c;
}

float4 colormap_grayscale(float x, float alpha) {
    return float4(colormap_gray_rgb(x), colormap_gray_rgb(x), colormap_gray_rgb(x), alpha);
}
