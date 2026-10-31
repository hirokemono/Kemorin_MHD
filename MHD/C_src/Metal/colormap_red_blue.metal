/*
//  colormap_red_blue.metal
//    float4 colormap_red_blue(float x, float alpha);
*/

#include <metal_stdlib>
using namespace metal;

#include "../GLSL/colormap_red_blue.frag"

float4 colormap_red_blue(float x, float alpha) {
    return float4(colormap_redblue_red(x), colormap_redblue_green(x), colormap_redblue_blue(x), alpha);
}
