/*
//
//  colormap_space.metal
//    float4 colormap_space(float x, float alpha);
*/

#include <metal_stdlib>

using namespace metal;

#include "../GLSL/colormap_space.frag"

float4 colormap_space(float x, float alpha) {
    return float4(colormap_space_red(x), colormap_space_green(x), colormap_space_blue(x), alpha);
}
