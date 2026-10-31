/*
//  colormap_red_blue.frag
//    vec4 colormap_red_blue(float x, float alpha);
*/

#define ABYSS_POINT  0.0
#define BLUE_POINT   0.1
#define WHITE_POINT  0.5
#define RED_POINT    0.9
#define BLOOD_POINT  1.0

float colormap_redblue_blue(float x) {
    float c = 0.0;
	if (x < ABYSS_POINT){
        c = 0.8;
	} else if (x >= ABYSS_POINT && x < BLUE_POINT){
        c = 0.8 + 2.0 * x;
	} else if (x >= BLUE_POINT && x < WHITE_POINT){
        c = 1.0 - (x - BLUE_POINT) * 0.25;
	} else if (x >= WHITE_POINT && x < RED_POINT){
        c = (RED_POINT - x) * 2.0;
	} else {
        c = 0.0;
	}
    return c;
}

float colormap_redblue_green(float x) {
    float c = 0.0;
	if (x < ABYSS_POINT){
        c = 0.2;
	} else if (x >= ABYSS_POINT && x < BLUE_POINT){
        c = 2.0 * (BLUE_POINT - x);
	} else if (x >= BLUE_POINT && x < WHITE_POINT){
        c = (x - BLUE_POINT) * 2.0;
	} else if (x >= WHITE_POINT && x < RED_POINT){
        c = (RED_POINT - x) * 2.0;
	} else {
        c = 0.0;
	}
    return c;
}

float colormap_redblue_red(float x) {
    float c = 0.0;
	if (x < BLUE_POINT){
        c = 0.0;
	} else if (x >= BLUE_POINT && x < WHITE_POINT){
        c = (x - BLUE_POINT) * 2.0;
	} else if (x >= WHITE_POINT && x < RED_POINT){
        c = 1.0 - (RED_POINT - x) * 0.25;
	} else if (x >= RED_POINT && x < BLOOD_POINT){
        c = 1.0 - (x - RED_POINT) * 2.0;
	} else {
        c = 0.8;
	}
    return c;
}
