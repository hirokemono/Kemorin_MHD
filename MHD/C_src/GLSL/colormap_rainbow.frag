/*
//  colormap_rainbow.frag
//    vec4 colormap_rainbow(float x);
*/

float colormap_rainbow_blue(float x) {
	float ocean =  0.325;
    float green =  0.55;
        
	if (x < ocean){
		return 1.0;
	} else if (x >= ocean && x < green){
		return 1.0 - 40.0*(x-ocean) / 9.0;
	} else {
		return 0.0;
	}
}

float colormap_rainbow_green(float x) {
    float blue =   0.1;
    float ocean =  0.325;
    float yellow = 0.775;
    float red =    1.0;
			
	if (x < blue){
		return 0.0;
	} else if (x >= blue && x < ocean){
		return 40.0*(x-blue) / 9.0;
	} else if (x >= ocean && x < yellow){
		return 1.0;
	} else if (x >= yellow && x < red){
		return 1.0 - 40.0*(x-yellow) / 9.0;
	} else {
		return 0.0;
	}
}

float colormap_rainbow_red(float x) {
    float purple = 0.0;
    float blue =   0.1;
    float green =  0.55;
    float yellow = 0.775;
			
	if (x < purple){
		return 0.5;
	} else if (x >= purple && x < blue){
		return 0.5 - 5.0*x;
	} else if (x >= blue && x < green){
		return 0.0;
	} else if (x >= green && x < yellow){
		return 40.0*(x-green) / 9.0;
	} else {
		return 1.0;
	}
}

vec4 colormap_rainbow(float x) {
    return vec4(colormap_rainbow_red(x), colormap_rainbow_green(x), colormap_rainbow_blue(x), 1.0);
}
