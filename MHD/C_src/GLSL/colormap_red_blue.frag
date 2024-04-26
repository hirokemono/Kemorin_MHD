/*
//  colormap_red_blue.frag
//    vec4 colormap_red_blue(float x, float alpha);
*/

float colormap_redblue_blue(float x) {
	float abyss =   ZERO;
	float blue =     0.1;
	float white =  ONE / TWO;
	float red =      0.9;
	
    float c = 0.0;
	if (x < abyss){
        c = 0.8;
	} else if (x >= abyss && x < blue){
        c = 0.8 + 2.0 * x;
	} else if (x >= blue && x < white){
        c = ONE - (x - blue) * 0.25;
	} else if (x >= white && x < red){
        c = (red - x) * 2.0;
	} else {
        c = ZERO;
	}
    return c;
}

float colormap_redblue_green(float x) {
	float abyss =   ZERO;
	float blue =     0.1;
	float white =  ONE / TWO;
	float red =      0.9;
	
    float c = 0.0;
	if (x < abyss){
        c = 0.2;
	} else if (x >= abyss && x < blue){
        c = 2.0 * (blue - x);
	} else if (x >= blue && x < white){
        c = (x - blue) * 2.0;
	} else if (x >= white && x < red){
        c = (red - x) * 2.0;
	} else {
        c = ZERO;
	}
    return c;
}

float colormap_redblue_red(float x) {
	float blue =     0.1;
	float white =  ONE / TWO;
	float red =      0.9;
	float blood =    ONE;
	
    float c = 0.0;
	if (x < blue){
        c = ZERO;
	} else if (x >= blue && x < white){
        c = (x - blue) * 2.0;
	} else if (x >= white && x < red){
        c = ONE - (red - x) * 0.25;
	} else if (x >= red && x < blood){
        c = ONE - (x - red) * 2.0;
	} else {
        c = 0.8;
	}
    return c;
}

vec4 colormap_red_blue(float x, float alpha) {
    return vec4(colormap_redblue_red(x), colormap_redblue_green(x), colormap_redblue_blue(x), alpha);
}
