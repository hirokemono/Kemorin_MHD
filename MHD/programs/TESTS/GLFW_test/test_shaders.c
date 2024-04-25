/*
//  Source of shader texts
//  Generated from shader files
*/

#include "test_shaders.h"

char * load_color_normalize_frag(void){
    const char  color_normalize_frag_src[]
    = {
        "/*\n"\
        "// color_normalize.frag\n"\
        "//  float color_normalize(uniform int num_tbl,\n"\
        "//                      uniform float d_in[16],\n"\
        "//                      uniform float d_norm[16],\n"\
        "//                        float x)\n"\
        "*/\n"\
        "\n"\
        "struct NormalizationTable{\n"\
        "    float xd;              // data value\n"\
        "    float xc;       // normalizad value\n"\
        "};\n"\
        "\n"\
        "float color_normalize(uniform int num_tbl,\n"\
        "                      uniform float d_in[16],\n"\
        "                      uniform float d_norm[16],\n"\
        "                      float x)\n"\
        "{\n"\
        "    float c;\n"\
        "\n"\
        "    if(x < d_in[0]){\n"\
        "        c = d_norm[0];\n"\
        "    }else if(x >= d_in[num_tbl-1]){\n"\
        "        c = d_norm[num_tbl-1];\n"\
        "\n"\
        "    }else if(x >= d_in[ 0] && x < d_in[ 1]){\n"\
        "        c = d_norm[ 0] + (d_norm[ 1] - d_norm[ 0])\n"\
        "            * (x - d_in[ 0]) / (d_in[ 1] - d_in[ 0]);\n"\
        "    }else if(x >= d_in[ 1] && x < d_in[ 2]){\n"\
        "        c = d_norm[ 1] + (d_norm[ 2] - d_norm[ 1])\n"\
        "            * (x - d_in[ 1]) / (d_in[ 2] - d_in[ 1]);\n"\
        "    }else if(x >= d_in[ 2] && x < d_in[ 3]){\n"\
        "        c = d_norm[ 2] + (d_norm[ 3] - d_norm[ 2])\n"\
        "            * (x - d_in[ 2]) / (d_in[ 3] - d_in[ 2]);\n"\
        "    }else if(x >= d_in[ 3] && x < d_in[ 4]){\n"\
        "        c = d_norm[ 3] + (d_norm[ 4] - d_norm[ 3])\n"\
        "            * (x - d_in[ 3]) / (d_in[ 4] - d_in[ 3]);\n"\
        "    }else if(x >= d_in[ 4] && x < d_in[ 5]){\n"\
        "        c = d_norm[ 4] + (d_norm[ 5] - d_norm[ 4])\n"\
        "            * (x - d_in[ 4]) / (d_in[ 5] - d_in[ 4]);\n"\
        "    }else if(x >= d_in[ 5] && x < d_in[ 6]){\n"\
        "        c = d_norm[ 5] + (d_norm[ 6] - d_norm[ 5])\n"\
        "            * (x - d_in[ 5]) / (d_in[ 6] - d_in[ 5]);\n"\
        "    }else if(x >= d_in[ 6] && x < d_in[ 7]){\n"\
        "        c = d_norm[ 6] + (d_norm[ 7] - d_norm[ 6])\n"\
        "            * (x - d_in[ 6]) / (d_in[ 7] - d_in[ 6]);\n"\
        "    }else if(x >= d_in[ 7] && x < d_in[ 8]){\n"\
        "        c = d_norm[ 7] + (d_norm[ 8] - d_norm[ 7])\n"\
        "            * (x - d_in[ 7]) / (d_in[ 8] - d_in[ 7]);\n"\
        "    }else if(x >= d_in[ 8] && x < d_in[ 9]){\n"\
        "        c = d_norm[ 8] + (d_norm[ 9] - d_norm[ 8])\n"\
        "            * (x - d_in[ 8]) / (d_in[ 9] - d_in[ 8]);\n"\
        "    }else if(x >= d_in[ 9] && x < d_in[10]){\n"\
        "        c = d_norm[ 9] + (d_norm[10] - d_norm[ 9])\n"\
        "            * (x - d_in[ 9]) / (d_in[10] - d_in[ 9]);\n"\
        "    }else if(x >= d_in[10] && x < d_in[11]){\n"\
        "        c = d_norm[10] + (d_norm[11] - d_norm[10])\n"\
        "            * (x - d_in[10]) / (d_in[11] - d_in[10]);\n"\
        "    }else if(x >= d_in[11] && x < d_in[12]){\n"\
        "        c = d_norm[11] + (d_norm[12] - d_norm[11])\n"\
        "            * (x - d_in[11]) / (d_in[12] - d_in[11]);\n"\
        "    }else if(x >= d_in[12] && x < d_in[13]){\n"\
        "        c = d_norm[12] + (d_norm[13] - d_norm[12])\n"\
        "            * (x - d_in[12]) / (d_in[13] - d_in[12]);\n"\
        "    }else if(x >= d_in[13] && x < d_in[14]){\n"\
        "        c = d_norm[13] + (d_norm[14] - d_norm[13])\n"\
        "            * (x - d_in[13]) / (d_in[14] - d_in[13]);\n"\
        "    }else if(x >= d_in[14] && x < d_in[15]){\n"\
        "        c = d_norm[14] + (d_norm[15] - d_norm[14])\n"\
        "            * (x - d_in[14]) / (d_in[15] - d_in[14]);\n"\
        "    }else{\n"\
        "        c = d_norm[table.num_tbl-1];\n"\
        "    };\n"\
        "    return c;\n"\
        "};\n"\
        "\n"
    };
    
    long n = strlen(color_normalize_frag_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, color_normalize_frag_src);
    return src;
};

char * load_colormap_grayscale_frag(void){
    const char  colormap_grayscale_frag_src[]
    = {
        "/*\n"\
        "//  colormap_grayscale.frag\n"\
        "//    vec4 colormap_grayscale(float x, float alpha);\n"\
        "*/\n"\
        "\n"\
        "float colormap_gray_rgb(float x) {\n"\
        "    float c;\n"\
        "	if (x < 0.0){\n"\
        "		c = 0.0;\n"\
        "	} else if (x > 1.0){\n"\
        "        c = 1.0;\n"\
        "	} else {\n"\
        "        c = 1.0*x;\n"\
        "	}\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "vec4 colormap_grayscale(float x, float alpha) {\n"\
        "    return vec4(colormap_gray_rgb(x), colormap_gray_rgb(x), colormap_gray_rgb(x), alpha);\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(colormap_grayscale_frag_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, colormap_grayscale_frag_src);
    return src;
};

char * load_colormap_molten_metal_frag(void){
    const char  colormap_molten_metal_frag_src[]
    = {
        "/*\n"\
        "//  colormap_molten_metal.frag\n"\
        "//    vec4 colormap_molten_metal(float x, float alpha);\n"\
        "*/\n"\
        "\n"\
        "float colormap_metal_blue(float x) {\n"\
        "    return 0.0;\n"\
        "}\n"\
        "\n"\
        "float colormap_metal_green(float x) {\n"\
        "    float c;\n"\
        "    if (x < 0.0) {\n"\
        "        c = 0.0;\n"\
        "    } else if (x <= 0.57147) {\n"\
        "        c = 446.22 * x / 255.0;\n"\
        "    } else {\n"\
        "        c = 1.0;\n"\
        "    }\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "float colormap_metal_red(float x) {\n"\
        "    float c;\n"\
        "    if (x < 0.6) {\n"\
        "        c = 0.0;\n"\
        "    } else if (x <= 0.95) {\n"\
        "        c = ((x - 0.6) * 728.57) / 255.0;\n"\
        "    } else {\n"\
        "        c = 1.0;\n"\
        "    }\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "vec4 colormap_molten_metal(float x, float alpha) {\n"\
        "    return vec4(colormap_metal_red(x), colormap_metal_green(x), colormap_metal_blue(x), alpha);\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(colormap_molten_metal_frag_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, colormap_molten_metal_frag_src);
    return src;
};

char * load_colormap_orange_cyan_frag(void){
    const char  colormap_orange_cyan_frag_src[]
    = {
        "/*\n"\
        "//  colormap_orange_cyan.frag\n"\
        "//    vec4 colormap_orange_cyan(float x);\n"\
        "*/\n"\
        "\n"\
        "float colormap_orangecyan_blue(float x) {\n"\
        "    float c;\n"\
        "    if (x < 0.0){\n"\
        "        c = 1.0;\n"\
        "    }else if(x < 0.5){\n"\
        "        c = (1.0 - x * 0.5);\n"\
        "    }else if(x < 1.0){\n"\
        "        c = (red - x) * 1.5;\n"\
        "    }else{\n"\
        "        c = 0.0;\n"\
        "    };\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "float colormap_orangecyan_green(float x, float alpha) {\n"\
        "    float c;\n"\
        "    if (x < 0.0){\n"\
        "        c = 1.0;\n"\
        "    }else if(x < 0.5){\n"\
        "        c = 1.0;\n"\
        "    }else if(x < 1.0){\n"\
        "        c = (red - x) + 0.5;\n"\
        "    }else{\n"\
        "        c = 0.5;\n"\
        "    };\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "float colormap_orangecyan_red(float x) {\n"\
        "    float c;\n"\
        "    if (x < 0.0){\n"\
        "        c = 0.0;\n"\
        "    }else if(x < 0.5){\n"\
        "        c = x * 2.0;\n"\
        "    }else if(x < 1.0){\n"\
        "        c = 1.0;\n"\
        "    }else{\n"\
        "        c = 1.0;\n"\
        "    };\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "vec4 colormap_orange_cyan(float x, float alpha) {\n"\
        "    return vec4(colormap_orangecyan_red(x), colormap_orangecyan_green(x), colormap_orangecyan_blue(x), alpha);\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(colormap_orange_cyan_frag_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, colormap_orange_cyan_frag_src);
    return src;
};

char * load_colormap_rainbow_frag(void){
    const char  colormap_rainbow_frag_src[]
    = {
        "/*\n"\
        "//  colormap_rainbow.frag\n"\
        "//    vec4 colormap_rainbow(float x, float alpha);\n"\
        "*/\n"\
        "\n"\
        "float colormap_rainbow_blue(float x) {\n"\
        "	float ocean =  0.325;\n"\
        "    float green =  0.55;\n"\
        "\n"\
        "    float c;\n"\
        "	if (x < ocean){\n"\
        "		c = 1.0;\n"\
        "	} else if (x >= ocean && x < green){\n"\
        "        c = 1.0 - 40.0*(x-ocean) / 9.0;\n"\
        "	} else {\n"\
        "        c = 0.0;\n"\
        "	}\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "float colormap_rainbow_green(float x) {\n"\
        "    float blue =   0.1;\n"\
        "    float ocean =  0.325;\n"\
        "    float yellow = 0.775;\n"\
        "    float red =    1.0;\n"\
        "			\n"\
        "    float c;\n"\
        "	if (x < blue){\n"\
        "        c = 0.0;\n"\
        "	} else if (x >= blue && x < ocean){\n"\
        "        c = 40.0*(x-blue) / 9.0;\n"\
        "	} else if (x >= ocean && x < yellow){\n"\
        "        c = 1.0;\n"\
        "	} else if (x >= yellow && x < red){\n"\
        "        c = 1.0 - 40.0*(x-yellow) / 9.0;\n"\
        "	} else {\n"\
        "        c = 0.0;\n"\
        "	}\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "float colormap_rainbow_red(float x) {\n"\
        "    float purple = 0.0;\n"\
        "    float blue =   0.1;\n"\
        "    float green =  0.55;\n"\
        "    float yellow = 0.775;\n"\
        "			\n"\
        "    float c;\n"\
        "	if (x < purple){\n"\
        "        c = 0.5;\n"\
        "	} else if (x >= purple && x < blue){\n"\
        "        c = 0.5 - 5.0*x;\n"\
        "	} else if (x >= blue && x < green){\n"\
        "        c = 0.0;\n"\
        "	} else if (x >= green && x < yellow){\n"\
        "        c = 40.0*(x-green) / 9.0;\n"\
        "	} else {\n"\
        "        c = 1.0;\n"\
        "	}\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "vec4 colormap_rainbow(float x, float alpha) {\n"\
        "    return vec4(colormap_rainbow_red(x), colormap_rainbow_green(x), colormap_rainbow_blue(x), alpha);\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(colormap_rainbow_frag_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, colormap_rainbow_frag_src);
    return src;
};

char * load_colormap_red_blue_frag(void){
    const char  colormap_red_blue_frag_src[]
    = {
        "/*\n"\
        "//  colormap_red_blue.frag\n"\
        "//    vec4 colormap_red_blue(float x, float alpha);\n"\
        "*/\n"\
        "\n"\
        "float colormap_redblue_blue(float x) {\n"\
        "	float abyss =   ZERO;\n"\
        "	float blue =     0.1;\n"\
        "	float white =  ONE / TWO;\n"\
        "	float red =      0.9;\n"\
        "	\n"\
        "    float c;\n"\
        "	if (x < abyss){\n"\
        "        c = 0.8;\n"\
        "	} else if (x >= abyss && x < blue){\n"\
        "        c = 0.8 + 2.0 * x;\n"\
        "	} else if (x >= blue && x < white){\n"\
        "        c = ONE - (x - blue) * 0.25;\n"\
        "	} else if (x >= white && x < red){\n"\
        "        c = (red - x) * 2.0;\n"\
        "	} else {\n"\
        "        c = ZERO;\n"\
        "	}\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "float colormap_redblue_green(float x) {\n"\
        "	float abyss =   ZERO;\n"\
        "	float blue =     0.1;\n"\
        "	float white =  ONE / TWO;\n"\
        "	float red =      0.9;\n"\
        "	\n"\
        "    float c;\n"\
        "	if (x < abyss){\n"\
        "        c = 0.2;\n"\
        "	} else if (x >= abyss && x < blue){\n"\
        "        c = 2.0 * (blue - x);\n"\
        "	} else if (x >= blue && x < white){\n"\
        "        c = (x - blue) * 2.0;\n"\
        "	} else if (x >= white && x < red){\n"\
        "        c = (red - x) * 2.0;\n"\
        "	} else {\n"\
        "        c = ZERO;\n"\
        "	}\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "float colormap_redblue_red(float x) {\n"\
        "	float blue =     0.1;\n"\
        "	float white =  ONE / TWO;\n"\
        "	float red =      0.9;\n"\
        "	float blood =    ONE;\n"\
        "	\n"\
        "    float c;\n"\
        "	if (x < blue){\n"\
        "        c = ZERO;\n"\
        "	} else if (x >= blue && x < white){\n"\
        "        c = (x - blue) * 2.0;\n"\
        "	} else if (x >= white && x < red){\n"\
        "        c = ONE - (red - x) * 0.25;\n"\
        "	} else if (x >= red && x < blood){\n"\
        "        c = ONE - (x - red) * 2.0;\n"\
        "	} else {\n"\
        "        c = 0.8;\n"\
        "	}\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "vec4 colormap_red_blue(float x, float alpha) {\n"\
        "    return vec4(colormap_redblue_red(x), colormap_redblue_green(x), colormap_redblue_blue(x), alpha);\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(colormap_red_blue_frag_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, colormap_red_blue_frag_src);
    return src;
};

char * load_colormap_select_frag(void){
    const char  colormap_select_frag_src[]
    = {
        "/*\n"\
        "//  colormap_select.frag\n"\
        "//    vec4 colormap_select(uniform int id_cmap, float x, float alpha);\n"\
        "*/\n"\
        "\n"\
        "#define RAINBOW_MODE    0\n"\
        "#define GRAYSCALE_MODE  1\n"\
        "#define RED_BLUE_MODE   2\n"\
        "#define SYM_GRAY_MODE   3\n"\
        "#define ORANGE_CYAN_MODE   4\n"\
        "#define MOLTEN_METAL_MODE  5\n"\
        "#define SPACE_COLOR_MODE   6\n"\
        "\n"\
        "struct KemoViewNormalize{\n"\
        "    float data_reference[16];             // Data\n"\
        "    float data_normalized[16];             // normalize\n"\
        "    int num_normalize;\n"\
        "\n"\
        "    float alpha_reference[16];             // Data\n"\
        "    float alpha_output[16];             // normalize\n"\
        "    int num_opacity;\n"\
        "\n"\
        "    int id_cmap;\n"\
        "}\n"\
        "\n"\
        "\n"\
        "vec4 colormap_select(uniform int id_cmap, float x, float alpha){\n"\
        "    vec4 c;\n"\
        "    if(id_cmap == RAINBOW_MODE){\n"\
        "        c = colormap_rainbow(x, alpha);\n"\
        "    }else if(id_cmap == GRAYSCALE_MODE){\n"\
        "        c = colormap_grayscale(x, alpha);\n"\
        "    }else if(id_cmap == RED_BLUE_MODE){\n"\
        "        c = colormap_red_blue(x, alpha);\n"\
        "    }else if(id_cmap == SYM_GRAY_MODE){\n"\
        "        c = colormap_sym_grayscale(x, alpha);\n"\
        "    }else if(id_cmap == ORANGE_CYAN_MODE){\n"\
        "        c = colormap_orange_cyan(x, alpha);\n"\
        "    }else if(id_cmap == MOLTEN_METAL_MODE){\n"\
        "        c = colormap_molten_metal(x, alpha);\n"\
        "    }else if(id_cmap == SPACE_COLOR_MODE){\n"\
        "        c = colormap_space(x, alpha);\n"\
        "    }else{\n"\
        "        c = colormap_rainbow(x, alpha);\n"\
        "    }\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "vec4 color_from_scalar(uniform KemoViewNormalize colormap, float x)\n"\
        "{\n"\
        "    float dataNorm = color_normalize(colormap.num_normalize,\n"\
        "                                     colormap.data_reference, colormap.data_normalized, x);\n"\
        "    float alpha =  color_normalize(colormap.num_opacity,\n"\
        "                                   colormap.alpha_reference, colormap.alpha_output, x);\n"\
        "    return colormap_select(colormap.id_cmap, dataNorm, alpha);\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(colormap_select_frag_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, colormap_select_frag_src);
    return src;
};

char * load_colormap_space_frag(void){
    const char  colormap_space_frag_src[]
    = {
        "//\n"\
        "// colormap_space.frag\n"\
        "//      vec4 colormap_space(float x, float alpha);\n"\
        "//\n"\
        "\n"\
        "float colormap_space_red(float x) {\n"\
        "    float c;\n"\
        "    if (x < 37067.0 / 158860.0) {\n"\
        "        c = 0.0;\n"\
        "    } else if (x < 85181.0 / 230350.0) {\n"\
        "        float xx = x - 37067.0 / 158860.0;\n"\
        "        c = (780.25 * xx + 319.71) * xx / 255.0;\n"\
        "    } else if (x < (sqrt(3196965649.0) + 83129.0) / 310480.0) {\n"\
        "        c = ((1035.33580904442 * x - 82.5380748768798) * x - 52.8985266363332) / 255.0;\n"\
        "    } else if (x < 231408.0 / 362695.0) {\n"\
        "        c = (339.41 * x - 33.194) / 255.0;\n"\
        "    } else if (x < 152073.0 / 222340.0) {\n"\
        "        c = (1064.8 * x - 496.01) / 255.0;\n"\
        "    } else if (x < 294791.0 / 397780.0) {\n"\
        "        c = (397.78 * x - 39.791) / 255.0;\n"\
        "    } else if (x < 491189.0 / 550980.0) {\n"\
        "        c = 1.0;\n"\
        "    } else if (x < 1.0) {\n"\
        "        c = (5509.8 * x + 597.91) * x / 255.0;\n"\
        "    } else {\n"\
        "        c = 1.0;\n"\
        "    }\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "float colormap_space_green(float x) {\n"\
        "    float xx;\n"\
        "    float c;\n"\
        "    if (x < 0.0) {\n"\
        "        c = 0.0;\n"\
        "    } else if (x < (-sqrt(166317494.0) + 39104.0) / 183830.0) {\n"\
        "        c = (-1838.3 * x + 464.36) * x / 255.0;\n"\
        "    } else if (x < 37067.0 / 158860.0) {\n"\
        "        c = (-317.72 * x + 74.134) / 255.0;\n"\
        "    } else if (x < (3.0 * sqrt(220297369.0) + 58535.0) / 155240.0) {\n"\
        "        c = 0.0;\n"\
        "    } else if (x < 294791.0 / 397780.0) {\n"\
        "        xx = x - (3.0 * sqrt(220297369.0) + 58535.0) / 155240.0;\n"\
        "        c = (-1945.0 * xx + 1430.2) * xx / 255.0;\n"\
        "    } else if (x < 491189.0 / 550980.0) {\n"\
        "        c = ((-1770.0 * x + 3.92813840044638e3) * x - 1.84017494792245e3) / 255.0;\n"\
        "    } else {\n"\
        "        c = 1.0;\n"\
        "    }\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "float colormap_space_blue(float x) {\n"\
        "    float c;\n"\
        "    if (x < 0.0) {\n"\
        "        c = 0.0;\n"\
        "    } else if (x < 51987.0 / 349730.0) {\n"\
        "        c = (458.79 * x) / 255.0;\n"\
        "    } else if (x < 85181.0 / 230350.0) {\n"\
        "        c = (109.06 * x + 51.987) / 255.0;\n"\
        "    } else if (x < (sqrt(3196965649.0) + 83129.0) / 310480.0) {\n"\
        "        c = (339.41 * x - 33.194) / 255.0;\n"\
        "    } else if (x < (3.0 * sqrt(220297369.0) + 58535.0) / 155240.0) {\n"\
        "        c = ((-1552.4 * x + 1170.7) * x - 92.996) / 255.0;\n"\
        "    } else if (x < 27568.0 / 38629.0) {\n"\
        "        c = 0.0;\n"\
        "    } else if (x < 81692.0 / 96241.0) {\n"\
        "        c = (386.29 * x - 275.68) / 255.0;\n"\
        "    } else if (x <= 1.0) {\n"\
        "        c = (1348.7 * x - 1092.6) / 255.0;\n"\
        "    } else {\n"\
        "        c = 1.0;\n"\
        "    }\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "vec4 colormap_space(float x, float alpha) {\n"\
        "    return vec4(colormap_space_red(x), colormap_space_green(x), colormap_space_blue(x), alpha);\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(colormap_space_frag_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, colormap_space_frag_src);
    return src;
};

char * load_colormap_sym_grayscale_frag(void){
    const char  colormap_sym_grayscale_frag_src[]
    = {
        "/*\n"\
        "//  colormap_sym_grayscale.frag\n"\
        "//    vec4 colormap_sym_grayscale(float x, float alpha);\n"\
        "*/\n"\
        "\n"\
        "float colormap_sym_gray_rgb(float x) {\n"\
        "    float c;\n"\
        "	if (x < 0.0){\n"\
        "        c = 0.0;\n"\
        "	} else if (x > 1.0){\n"\
        "        c = 0.0;\n"\
        "	} else if (x >= 0.0 && x < 0.5){\n"\
        "        c = 1.0*x;\n"\
        "	} else {\n"\
        "        c = 1.0*(1.0 - x);\n"\
        "	}\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "vec4 colormap_sym_grayscale(float x, float alpha) {\n"\
        "    return vec4(colormap_sym_gray_rgb(x), colormap_sym_gray_rgb(x), colormap_sym_gray_rgb(x), alpha);\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(colormap_sym_grayscale_frag_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, colormap_sym_grayscale_frag_src);
    return src;
};

char * load_phong_w_colormap_frag(void){
    const char  phong_w_colormap_frag_src[]
    = {
        "#version 330\n"\
        "// phong.frag\n"\
        "\n"\
        "in vec4 position;\n"\
        "in vec4 ex_Color;\n"\
        "in vec4 normal;\n"\
        "out vec4 out_Color;\n"\
        "\n"\
        "#define MAX_LIGHTS 10\n"\
        "struct LightSourceParameters{\n"\
        "	vec4 ambient;              // Aclarri\n"\
        "	vec4 diffuse;              // Dcli\n"\
        "	vec4 specular;             // Scli\n"\
        "	vec4 position;             // Ppli\n"\
        "	vec4 halfVector;           // Derived: Hi\n"\
        "	vec3 spotDirection;        // Sdli\n"\
        "	float spotExponent;        // Srli\n"\
        "	float spotCutoff;          // Crli\n"\
        "	// (range: [0.0,90.0], 180.0)\n"\
        "	float spotCosCutoff;       // Derived: cos(Crli)\n"\
        "	// (range: [1.0,0.0],-1.0)\n"\
        "	float constantAttenuation;   // K0\n"\
        "	float linearAttenuation;     // K1\n"\
        "	float quadraticAttenuation;  // K2\n"\
        "};\n"\
        "uniform int num_lights;\n"\
        "uniform LightSourceParameters LightSource[MAX_LIGHTS];\n"\
        "\n"\
        "struct ColorMaterial {\n"\
        "	vec4 emission;    // Ecm\n"\
        "	vec4 ambient;     // Acm\n"\
        "	vec4 diffuse;     // Dcm\n"\
        "	vec4 specular;    // Scm\n"\
        "	float shininess;  // Srm\n"\
        "};\n"\
        "uniform ColorMaterial frontMaterial;\n"\
        "uniform ColorMaterial backMaterial;\n"\
        "\n"\
        "void main (void)\n"\
        "{\n"\
        "	vec3 fnormal = normalize(normal.xyz);\n"\
        "	vec3 light;\n"\
        "	float diffuse;\n"\
        "\n"\
        "    vec3 halfway;\n"\
        "    float product;\n"\
        "    float fspecular;\n"\
        "\n"\
        "    vec3 view =   normalize(position.xyz);\n"\
        "    vec4 tmpsp =  vec4(frontMaterial.specular.xyz, ex_Color.w);\n"\
        "\n"\
        "	out_Color = vec4(0.0,0.0,0.0,0.0);\n"\
        "	for (int i = 0; i < num_lights; ++i){\n"\
        "		light = normalize(LightSource[i].position.xyz - position.xyz);\n"\
        "\n"\
        "        halfway =   normalize(light - view);\n"\
        "        product =   max(dot(fnormal, halfway), 0.0);\n"\
        "        fspecular = pow(product, frontMaterial.shininess);\n"\
        "		diffuse =   dot(light, fnormal);\n"\
        "\n"\
        "		out_Color += ex_Color * frontMaterial.ambient\n"\
        "                    + ex_Color * frontMaterial.diffuse * abs(diffuse)\n"\
        "                    + tmpsp * fspecular;\n"\
        "//		if (diffuse > 0.0) {\n"\
        "//			out_Color += ex_Color * frontMaterial.diffuse * diffuse\n"\
        "//                        + tmpsp * fspecular;\n"\
        "//		}\n"\
        "	}\n"\
        "}\n"\
        "\n"\
        "\n"
    };
    
    long n = strlen(phong_w_colormap_frag_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, phong_w_colormap_frag_src);
    return src;
};

char * load_phong_w_colormap_vert(void){
    const char  phong_w_colormap_vert_src[]
    = {
        "// #version 330\n"\
        "//\n"\
        "// phong.vert\n"\
        "//\n"\
        "\n"\
        "layout (location = 0) in vec4  xyz;\n"\
        "layout (location = 1) in vec4  color;\n"\
        "layout (location = 2) in vec4  norm;\n"\
        "layout (location = 3) in vec2  txur;\n"\
        "layout (location = 4) in vec2  data;\n"\
        "\n"\
        "\n"\
        "uniform mat4 projectionMat;\n"\
        "uniform mat4 viewMatrix;\n"\
        "uniform mat4 modelViewMat;\n"\
        "uniform mat4 modelNormalMat;\n"\
        "uniform KemoViewNormalize colormap;\n"\
        "\n"\
        "out vec4  position;\n"\
        "out vec4  normal;\n"\
        "out vec4  ex_Color;\n"\
        "\n"\
        "void main(void)\n"\
        "{\n"\
        "    position = xyz;\n"\
        "    position.w = 1.0;\n"\
        "    position = modelViewMat * position;\n"\
        "	normal =   modelNormalMat * norm;\n"\
        "    ex_Color = color_from_scalar(colormap, data.x);\n"\
        "\n"\
        "	gl_Position =  projectionMat * position;\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(phong_w_colormap_vert_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, phong_w_colormap_vert_src);
    return src;
};

