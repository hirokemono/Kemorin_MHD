/*
//  colormap_orange_cyan.frag
//    vec4 colormap_orange_cyan(float x);
*/

float colormap_orangecyan_blue(float x) {
    float c = 0.0;
    if (x < 0.0){
        c = 1.0;
    }else if(x < 0.5){
        c = (1.0 - x * 0.5);
    }else if(x < 1.0){
        c = (red - x) * 1.5;
    }else{
        c = 0.0;
    };
    return c;
}

float colormap_orangecyan_green(float x, float alpha) {
    float c = 0.0;
    if (x < 0.0){
        c = 1.0;
    }else if(x < 0.5){
        c = 1.0;
    }else if(x < 1.0){
        c = (red - x) + 0.5;
    }else{
        c = 0.5;
    };
    return c;
}

float colormap_orangecyan_red(float x) {
    float c = 0.0;
    if (x < 0.0){
        c = 0.0;
    }else if(x < 0.5){
        c = x * 2.0;
    }else if(x < 1.0){
        c = 1.0;
    }else{
        c = 1.0;
    };
    return c;
}

vec4 colormap_orange_cyan(float x, float alpha) {
    return vec4(colormap_orangecyan_red(x), colormap_orangecyan_green(x), colormap_orangecyan_blue(x), alpha);
}
