/*
//  colormap_grayscale.frag
//    vec4 colormap_grayscale(float x, float alpha);
*/

float colormap_gray_rgb(float x) {
    float c;
	if (x < 0.0){
		c = 0.0;
	} else if (x > 1.0){
        c = 1.0;
	} else {
        c = 1.0*x;
	}
    return c;
}

vec4 colormap_grayscale(float x, float alpha) {
    return vec4(colormap_gray_rgb(x), colormap_gray_rgb(x), colormap_gray_rgb(x), alpha);
}
