/*
//  colormap_molten_metal.frag
//    vec4 colormap_molten_metal(float x);
*/

float colormap_metal_blue(float x) {
    return 0.0;
}

float colormap_metal_green(float x) {
    if (x < 0.0) {
        return 0.0;
    } else if (x <= 0.57147) {
        return 446.22 * x / 255.0;
    } else {
       return 1.0;
    }
}

float colormap_metal_red(float x) {
    if (x < 0.6) {
        return 0.0;
    } else if (x <= 0.95) {
        return ((x - 0.6) * 728.57) / 255.0;
    } else {
        return 1.0;
    }
}

vec4 colormap_molten_metal(float x) {
    return vec4(colormap_metal_red(x), colormap_metal_green(x), colormap_metal_blue(x), 1.0);
}
