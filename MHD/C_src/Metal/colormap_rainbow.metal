/*
//  colormap_rainbow.metal
//    float4 colormap_rainbow(float x, float alpha);
*/

#include <metal_stdlib>
using namespace metal;

#include "../GLSL/colormap_rainbow.frag"

float4 colormap_rainbow(float x, float alpha) {
    return float4(colormap_rainbow_red(x), colormap_rainbow_green(x), colormap_rainbow_blue(x), alpha);
}
