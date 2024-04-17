/*
//  colormap_grayscale.frag
//    vec4 colormap_grayscale(float x);
*/

float colormap_gray_rgb(float x) {
	if (x < 0.0){
		return 0.0;
	} else if (x > 1.0){
		return 1.0;
	} else {
		return 1.0*x;
	}
}

vec4 colormap_grayscale(float x) {
    return vec4(colormap_gray_rgb(x), colormap_gray_rgb(x), colormap_gray_rgb(x), 1.0);
}
