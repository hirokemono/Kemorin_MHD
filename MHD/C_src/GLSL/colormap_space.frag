//
// colormap_space.frag
//      vec4 colormap_space(float x, float alpha);
//

float colormap_space_red(float x) {
    float c = 0.0;
    if (x < 37067.0 / 158860.0) {
        c = 0.0;
    } else if (x < 85181.0 / 230350.0) {
        float xx = x - 37067.0 / 158860.0;
        c = (780.25 * xx + 319.71) * xx / 255.0;
    } else if (x < (sqrt(3196965649.0) + 83129.0) / 310480.0) {
        c = ((1035.33580904442 * x - 82.5380748768798) * x - 52.8985266363332) / 255.0;
    } else if (x < 231408.0 / 362695.0) {
        c = (339.41 * x - 33.194) / 255.0;
    } else if (x < 152073.0 / 222340.0) {
        c = (1064.8 * x - 496.01) / 255.0;
    } else if (x < 294791.0 / 397780.0) {
        c = (397.78 * x - 39.791) / 255.0;
    } else if (x < 491189.0 / 550980.0) {
        c = 1.0;
    } else if (x < 1.0) {
        c = (5509.8 * x + 597.91) * x / 255.0;
    } else {
        c = 1.0;
    }
    return c;
}

float colormap_space_green(float x) {
    float xx = 0.0;
    float c = 0.0;
    if (x < 0.0) {
        c = 0.0;
    } else if (x < (-sqrt(166317494.0) + 39104.0) / 183830.0) {
        c = (-1838.3 * x + 464.36) * x / 255.0;
    } else if (x < 37067.0 / 158860.0) {
        c = (-317.72 * x + 74.134) / 255.0;
    } else if (x < (3.0 * sqrt(220297369.0) + 58535.0) / 155240.0) {
        c = 0.0;
    } else if (x < 294791.0 / 397780.0) {
        xx = x - (3.0 * sqrt(220297369.0) + 58535.0) / 155240.0;
        c = (-1945.0 * xx + 1430.2) * xx / 255.0;
    } else if (x < 491189.0 / 550980.0) {
        c = ((-1770.0 * x + 3.92813840044638e3) * x - 1.84017494792245e3) / 255.0;
    } else {
        c = 1.0;
    }
    return c;
}

float colormap_space_blue(float x) {
    float c = 0.0;
    if (x < 0.0) {
        c = 0.0;
    } else if (x < 51987.0 / 349730.0) {
        c = (458.79 * x) / 255.0;
    } else if (x < 85181.0 / 230350.0) {
        c = (109.06 * x + 51.987) / 255.0;
    } else if (x < (sqrt(3196965649.0) + 83129.0) / 310480.0) {
        c = (339.41 * x - 33.194) / 255.0;
    } else if (x < (3.0 * sqrt(220297369.0) + 58535.0) / 155240.0) {
        c = ((-1552.4 * x + 1170.7) * x - 92.996) / 255.0;
    } else if (x < 27568.0 / 38629.0) {
        c = 0.0;
    } else if (x < 81692.0 / 96241.0) {
        c = (386.29 * x - 275.68) / 255.0;
    } else if (x <= 1.0) {
        c = (1348.7 * x - 1092.6) / 255.0;
    } else {
        c = 1.0;
    }
    return c;
}

vec4 colormap_space(float x, float alpha) {
    return vec4(colormap_space_red(x), colormap_space_green(x), colormap_space_blue(x), alpha);
}
