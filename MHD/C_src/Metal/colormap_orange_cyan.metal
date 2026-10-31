/*
//  colormap_orange_cyan.metal
//    float4 colormap_orange_cyan(float x);
*/

#include <metal_stdlib>
using namespace metal;

#include "../GLSL/colormap_orange_cyan.frag"

float4 colormap_orange_cyan(float x, float alpha) {
    return float4(colormap_orangecyan_red(x), colormap_orangecyan_green(x), colormap_orangecyan_blue(x), alpha);
}
