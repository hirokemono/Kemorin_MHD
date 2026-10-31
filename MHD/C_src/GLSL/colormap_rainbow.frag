/*
//  colormap_rainbow.frag
//    vec4 colormap_rainbow(float x, float alpha);
*/

#define PURPLE_POINT 0.0
#define BLUE_POINT   0.1
#define OCEAN_POINT  0.325
#define GREEN_POINT  0.55
#define YELLOW_POINT 0.775
#define RED_POINT    1.0



float colormap_rainbow_blue(float x) {
    float c = 0.0;
	if (x < OCEAN_POINT){
		c = 1.0;
	} else if (x >= OCEAN_POINT && x < GREEN_POINT){
        c = 1.0 - 40.0*(x-OCEAN_POINT) / 9.0;
	} else {
        c = 0.0;
	}
    return c;
}

float colormap_rainbow_green(float x) {
    float c = 0.0;
	if (x < BLUE_POINT){
        c = 0.0;
	} else if (x >= BLUE_POINT && x < OCEAN_POINT){
        c = 40.0*(x - BLUE_POINT) / 9.0;
	} else if (x >= OCEAN_POINT && x < YELLOW_POINT){
        c = 1.0;
	} else if (x >= YELLOW_POINT && x < RED_POINT){
        c = 1.0 - 40.0*(x - YELLOW_POINT) / 9.0;
	} else {
        c = 0.0;
	}
    return c;
}

float colormap_rainbow_red(float x) {
    float c = 0.0;
	if (x < PURPLE_POINT){
        c = 0.5;
	} else if (x >= PURPLE_POINT && x < BLUE_POINT){
        c = 0.5 - 5.0*x;
	} else if (x >= BLUE_POINT && x < GREEN_POINT){
        c = 0.0;
	} else if (x >= GREEN_POINT && x < YELLOW_POINT){
        c = 40.0*(x-GREEN_POINT) / 9.0;
	} else {
        c = 1.0;
	}
    return c;
}
