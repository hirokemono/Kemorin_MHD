/*
//  colormap_select.frag
//    vec4 colormap_select(uniform int id_cmap, float x, float alpha);
*/

#define RAINBOW_MODE    0
#define GRAYSCALE_MODE  1
#define RED_BLUE_MODE   2
#define SYM_GRAY_MODE   3
#define ORANGE_CYAN_MODE   4
#define MOLTEN_METAL_MODE  5
#define SPACE_COLOR_MODE   6


struct KemoViewNormalize{
    float data_reference[MAX_NORMALIZATION_POINT];             // Data
    float data_normalized[MAX_NORMALIZATION_POINT];             // normalize
    int num_normalize;

    float alpha_reference[MAX_NORMALIZATION_POINT];             // Data
    float alpha_output[MAX_NORMALIZATION_POINT];             // normalize
    int num_opacity;

    int id_cmap;
};

vec4 colormap_select(int id_cmap, float x, float alpha){
    vec4 c = vec4(0.0, 0.0, 0.0, 0.0);
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

vec4 color_from_scalar(KemoViewNormalize colormap, float x)
{
    float dataNorm = color_normalize(colormap.num_normalize,
                                     colormap.data_reference, colormap.data_normalized, x);
    float alpha =  color_normalize(colormap.num_opacity,
                                   colormap.alpha_reference, colormap.alpha_output, x);
    return colormap_select(colormap.id_cmap, dataNorm, alpha);
}
