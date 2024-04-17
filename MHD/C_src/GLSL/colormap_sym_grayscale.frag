/*
//  colormap_sym_grayscale.frag
//    vec4 colormap_sym_grayscale(float x);
*/

float colormap_sym_gray_rgb(float x) {
	if (x < 0.0){
		return 0.0;
	} else if (x > 1.0){
		return 0.0;
	} else if (x >= 0.0 && x < 0.5){
		return 1.0*x;
	} else {
		return 1.0*(1.0 - x);
	}
}

vec4 colormap_sym_grayscale(float x) {
    return vec4(colormap_sym_gray_rgb(x), colormap_sym_gray_rgb(x), colormap_sym_gray_rgb(x), 1.0);
}
