/*
//  colormap_select.metal
//     float4 color_from_scalar(constant KemoViewNormalize *normalizePointer,
//                              constant KemoViewNormalize *opacityPointer,
//                              constant int id_cmap, float x)
*/

#include <metal_stdlib>
// Include header shared between this Metal shader code and C code executing Metal API commands
#include "KemoViewShaderTypes.h"

float color_normalize(constant int num_tbl[1],
                      constant float d_in[16],
                      constant float d_norm[16],
                      float x);
float4 colormap_rainbow(float x, float alpha);
float4 colormap_grayscale(float x, float alpha);
float4 colormap_red_blue(float x, float alpha);
float4 colormap_sym_grayscale(float x, float alpha);
float4 colormap_orange_cyan(float x, float alpha);
float4 colormap_molten_metal(float x, float alpha);
float4 colormap_space(float x, float alpha);



float4 colormap_select(constant int id_cmap[1], float x, float alpha){
    float4 c = float4(0.0, 0.0, 0.0, 0.0);
    if(id_cmap[0] == RAINBOW_MODE){
        c = colormap_rainbow(x, alpha);
    }else if(id_cmap[0] == GRAYSCALE_MODE){
        c = colormap_grayscale(x, alpha);
    }else if(id_cmap[0] == RED_BLUE_MODE){
        c = colormap_red_blue(x, alpha);
    }else if(id_cmap[0] == SYM_GRAY_MODE){
        c = colormap_sym_grayscale(x, alpha);
    }else if(id_cmap[0] == ORANGE_CYAN_MODE){
        c = colormap_orange_cyan(x, alpha);
    }else if(id_cmap[0] == MOLTEN_METAL_MODE){
        c = colormap_molten_metal(x, alpha);
    }else if(id_cmap[0] == SPACE_COLOR_MODE){
        c = colormap_space(x, alpha);
    }else{
        c = colormap_rainbow(x, alpha);
    }
    return c;
}

float4 color_from_scalar(constant KemoViewNormalize *colorMapPointer, float x){
    float dataNorm = color_normalize(colorMapPointer->num_normalize,
                                     colorMapPointer->data_reference,
                                     colorMapPointer->data_normalized, x);
    float alpha =    color_normalize(colorMapPointer->num_opacity,
                                     colorMapPointer->alpha_reference,
                                     colorMapPointer->alpha_output, x);
    return colormap_select(colorMapPointer->id_cmap, dataNorm, alpha);
};


