/*
//  colormap_molten_metal.metal
//    float4 colormap_molten_metal(float x, float alpha);
*/

#include <metal_stdlib>
using namespace metal;

#include "../GLSL/colormap_molten_metal.frag"

float4 colormap_molten_metal(float x, float alpha) {
    return float4(colormap_metal_red(x), colormap_metal_green(x), 0.0, alpha);
}
