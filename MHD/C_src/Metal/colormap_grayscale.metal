/*
//  colormap_grayscale.metal
//    float4 colormap_grayscale(float x, float alpha);
*/

#include <metal_stdlib>

using namespace metal;

#include "../GLSL/colormap_grayscale.frag"

float4 colormap_grayscale(float x, float alpha) {
    float c = colormap_gray_rgb(x);
    return float4(c, c, c, alpha);
}
