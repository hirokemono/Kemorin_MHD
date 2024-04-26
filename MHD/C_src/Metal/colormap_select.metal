/*
//  colormap_select.metal
//     float4 color_from_scalar(constant KemoViewNormalize *normalizePointer,
//                              constant KemoViewNormalize *opacityPointer,
//                              constant int id_cmap, float x)
*/

#include <metal_stdlib>
// Include header shared between this Metal shader code and C code executing Metal API commands
#include "KemoViewShaderTypes.h"

/*

float color_normalize(uniform int num_tbl,
                      uniform float d_in[16],
                      uniform float d_norm[16],
                      float x);
float4 colormap_rainbow(float x, float alpha);
float4 colormap_grayscale(float x, float alpha);
float4 colormap_red_blue(float x, float alpha);
float4 colormap_sym_grayscale(float x, float alpha);
float4 colormap_orange_cyan(float x, float alpha);
float4 colormap_molten_metal(float x, float alpha);
float4 colormap_space(float x, float alpha);



float4 colormap_select(constant int id_cmap, float x, float alpha){
    float4 c = float4(0.0, 0.0, 0.0, 0.0);
    if(id_cmap == RAINBOW_MODE){
        c = colormap_rainbow(x, alpha);
    }else if(id_cmap == GRAYSCALE_MODE){
        c = colormap_grayscale(x, alpha);
    }else if(id_cmap == RED_BLUE_MODE){
        c = colormap_red_blue(x, alpha);
    }else if(id_cmap == SYM_GRAY_MODE){
        c = colormap_sym_grayscale(x, alpha);
    }else if(id_cmap == ORANGE_CYAN_MODE){
        c = colormap_orange_cyan(x, alpha);
    }else if(id_cmap == MOLTEN_METAL_MODE){
        c = colormap_molten_metal(x, alpha);
    }else if(id_cmap == SPACE_COLOR_MODE){
        c = colormap_space(x, alpha);
    }else{
        c = colormap_rainbow(x, alpha);
    }
    return c;
}

float4 color_from_scalar(constant KemoViewNormalize *normalizePointer, float x)
{
    KemoViewNormalize normalize = KemoViewNormalize(*normalizePointer);

    float dataNorm = color_normalize(normalize.num_normalize,
                                     normalize.data_reference, normalize.data_normalized, x);
    float alpha =  color_normalize(normalize.num_opacity,
                                   normalize.alpha_reference, normalize.alpha_output, x);
    return colormap_select(normalize.id_cmap, dataNorm, alpha);
}

*/
float4 color_from_scalar(float x){
    return float4(x*0.01, x*0.1, x*0.01, 1.0);
};


