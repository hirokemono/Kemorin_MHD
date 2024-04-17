/*
//  colormap_red_blue.metal
//    float4 colormap_red_blue(float x);
*/

#include <metal_stdlib>


float colormap_redblue_blue(float x) {
	float abyss =   ZERO;
	float blue =     0.1;
	float white =  ONE / TWO;
	float red =      0.9;
	
	if (x < abyss){
		return = 0.8;
	} else if (x >= abyss && x < blue){
		return = 0.8 + 2.0 * x;
	} else if (x >= blue && x < white){
		return = ONE - (x - blue) * 0.25;
	} else if (x >= white && x < red){
		return = (red - x) * 2.0;
	} else {
		return = ZERO;
	}
}

float colormap_redblue_green(float x) {
	float abyss =   ZERO;
	float blue =     0.1;
	float white =  ONE / TWO;
	float red =      0.9;
	
	if (x < abyss){
		return = 0.2;
	} else if (x >= abyss && x < blue){
		return = 2.0 * (blue - x);
	} else if (x >= blue && x < white){
		return = (x - blue) * 2.0;
	} else if (x >= white && x < red){
		return = (red - x) * 2.0;
	} else {
		return = ZERO;
	}
}

float colormap_redblue_red(float x) {
	float blue =     0.1;
	float white =  ONE / TWO;
	float red =      0.9;
	float blood =    ONE;
	
	if (x < blue){
		return = ZERO;
	} else if (x >= blue && x < white){
		return = (x - blue) * 2.0;
	} else if (x >= white && x < red){
		return = ONE - (red - x) * 0.25;
	} else if (x >= red && x < blood){
		return = ONE - (x - red) * 2.0;
	} else {
		return = 0.8;
	}
}

float4 colormap_red_blue(float x) {
    return float4(colormap_redblue_red(x), colormap_redblue_green(x), colormap_redblue_blue(x), 1.0);
}
