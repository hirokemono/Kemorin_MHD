/*
//  colormap_sym_grayscale.frag
//    vec4 colormap_sym_grayscale(float x, float alpha);
*/

float colormap_sym_gray_rgb(float x) {
    float c = 0.0;
	if (x < 0.0){
        c = 0.0;
	} else if (x > 1.0){
        c = 0.0;
	} else if (x >= 0.0 && x < 0.5){
        c = 1.0*x;
	} else {
        c = 1.0*(1.0 - x);
	}
    return c;
}

vec4 colormap_sym_grayscale(float x, float alpha) {
    return vec4(colormap_sym_gray_rgb(x), colormap_sym_gray_rgb(x), colormap_sym_gray_rgb(x), alpha);
}
