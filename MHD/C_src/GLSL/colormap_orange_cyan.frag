/*
//  colormap_orange_cyan.frag
//    vec4 colormap_orange_cyan(float x);
*/

float colormap_orangecyan_blue(float x) {
    if (x < 0.0){
        return 1.0;
    }else if(x < 0.5){
        return (1.0 - x * 0.5);
    }else if(x < 1.0){
        return (red - x) * 1.5;
    }else{
        return 0.0;
    };
}

float colormap_orangecyan_green(float x) {
    if (x < 0.0){
        return 1.0;
    }else if(x < 0.5){
        return 1.0;
    }else if(x < 1.0){
        return (red - x) + 0.5;
    }else{
        return 0.5;
    };
}

float colormap_orangecyan_red(float x) {
    if (x < 0.0){
        return 0.0;
    }else if(x < 0.5){
        return x * 2.0;
    }else if(x < 1.0){
        return 1.0;
    }else{
        return 1.0;
    };
}

vec4 colormap_orange_cyan(float x) {
    return vec4(colormap_orangecyan_red(x), colormap_orangecyan_green(x), colormap_orangecyan_blue(x), 1.0);
}
