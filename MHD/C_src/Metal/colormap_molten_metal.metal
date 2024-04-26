/*
//  colormap_molten_metal.metal
//    float4 colormap_molten_metal(float x, float alpha);
*/

#include <metal_stdlib>


float colormap_metal_blue(float x) {
    return 0.0;
}

float colormap_metal_green(float x) {
    float c = 0.0;
    if (x < 0.0) {
        c = 0.0;
    } else if (x <= 0.57147) {
        c = 446.22 * x / 255.0;
    } else {
        c = 1.0;
    }
    return c;
}

float colormap_metal_red(float x) {
    float c = 0.0;
    if (x < 0.6) {
        c = 0.0;
    } else if (x <= 0.95) {
        c = ((x - 0.6) * 728.57) / 255.0;
    } else {
        c = 1.0;
    }
    return c;
}

float4 colormap_molten_metal(float x, float alpha) {
    return float4(colormap_metal_red(x), colormap_metal_green(x), colormap_metal_blue(x), alpha);
}
