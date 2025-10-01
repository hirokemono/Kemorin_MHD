/*
//  Source of shader texts
//  Generated from shader files
*/

#include "shaders.h"

char * load_anaglyph_texture_frag(void){
    const char  anaglyph_texture_frag_src[]
    = {
        "// #version 330\n"\
        "//\n"\
        "// anaglyph_texture.frag\n"\
        "//\n"\
        "in vec2 tex_position;\n"\
        "\n"\
        "out vec4 out_Color;\n"\
        "\n"\
        "uniform sampler2D left_image;\n"\
        "uniform sampler2D right_image;\n"\
        "\n"\
        "void main (void)\n"\
        "{\n"\
        "    vec4 left_Color =  texture(left_image, tex_position);\n"\
        "    vec4 right_Color = texture(right_image, tex_position);\n"\
        "\n"\
        "    float r = 0.299 * left_Color.x + 0.587 * left_Color.y + 0.114 * left_Color.z;\n"\
        "    out_Color = vec4(r, right_Color.y, right_Color.z, left_Color.w);\n"\
        "}\n"\
        "\n"\
        "\n"
    };
    
    long n = strlen(anaglyph_texture_frag_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, anaglyph_texture_frag_src);
    return src;
};

char * load_anaglyph_texture_vert(void){
    const char  anaglyph_texture_vert_src[]
    = {
        "// #version 330\n"\
        "//\n"\
        "// anaglyph_texture.vert\n"\
        "//\n"\
        "layout (location = 0) in vec4  xyz;\n"\
        "layout (location = 1) in vec2  txur;\n"\
        "\n"\
        "\n"\
        "uniform mat4 projectionMat;\n"\
        "uniform mat4 modelViewMat;\n"\
        "\n"\
        "out vec2 tex_position;\n"\
        "\n"\
        "void main(void)\n"\
        "{\n"\
        "    vec4 position = xyz;\n"\
        "    position.w = 1.0;\n"\
        "	position = modelViewMat * position;\n"\
        "	tex_position = txur;\n"\
        "\n"\
        "	gl_Position =  projectionMat * position;\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(anaglyph_texture_vert_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, anaglyph_texture_vert_src);
    return src;
};

char * load_color_normalize_frag(void){
    const char  color_normalize_frag_src[]
    = {
        "/*\n"\
        "// color_normalize.frag\n"\
        "//     float color_normalize(int num_tbl,\n"\
        "//                           float d_in[MAX_NORMALIZATION_POINT],\n"\
        "//                           float d_norm[MAX_NORMALIZATION_POINT],\n"\
        "//                           float x)\n"\
        "*/\n"\
        "\n"\
        "#define MAX_NORMALIZATION_POINT 16\n"\
        "\n"\
        "float color_normalize(int num_tbl,\n"\
        "                      float d_in[MAX_NORMALIZATION_POINT],\n"\
        "                      float d_norm[MAX_NORMALIZATION_POINT],\n"\
        "                      float x){\n"\
        "    float c = 0.0;\n"\
        "    if(x < d_in[0]){\n"\
        "        c = d_norm[0];\n"\
        "    }else if(x >= d_in[num_tbl-1]){\n"\
        "        c = d_norm[num_tbl-1];\n"\
        "\n"\
        "    }else if(x < d_in[ 1]){\n"\
        "        c = d_norm[ 0] + (d_norm[ 1] - d_norm[ 0])\n"\
        "            * (x - d_in[ 0]) / (d_in[ 1] - d_in[ 0]);\n"\
        "    }else if(x < d_in[ 2]){\n"\
        "        c = d_norm[ 1] + (d_norm[ 2] - d_norm[ 1])\n"\
        "            * (x - d_in[ 1]) / (d_in[ 2] - d_in[ 1]);\n"\
        "    }else if(x < d_in[ 3]){\n"\
        "        c = d_norm[ 2] + (d_norm[ 3] - d_norm[ 2])\n"\
        "            * (x - d_in[ 2]) / (d_in[ 3] - d_in[ 2]);\n"\
        "    }else if(x < d_in[ 4]){\n"\
        "        c = d_norm[ 3] + (d_norm[ 4] - d_norm[ 3])\n"\
        "            * (x - d_in[ 3]) / (d_in[ 4] - d_in[ 3]);\n"\
        "    }else if(x < d_in[ 5]){\n"\
        "        c = d_norm[ 4] + (d_norm[ 5] - d_norm[ 4])\n"\
        "            * (x - d_in[ 4]) / (d_in[ 5] - d_in[ 4]);\n"\
        "    }else if(x < d_in[ 6]){\n"\
        "        c = d_norm[ 5] + (d_norm[ 6] - d_norm[ 5])\n"\
        "            * (x - d_in[ 5]) / (d_in[ 6] - d_in[ 5]);\n"\
        "    }else if(x < d_in[ 7]){\n"\
        "        c = d_norm[ 6] + (d_norm[ 7] - d_norm[ 6])\n"\
        "            * (x - d_in[ 6]) / (d_in[ 7] - d_in[ 6]);\n"\
        "    }else if(x < d_in[ 8]){\n"\
        "        c = d_norm[ 7] + (d_norm[ 8] - d_norm[ 7])\n"\
        "            * (x - d_in[ 7]) / (d_in[ 8] - d_in[ 7]);\n"\
        "    }else if(x < d_in[ 9]){\n"\
        "        c = d_norm[ 8] + (d_norm[ 9] - d_norm[ 8])\n"\
        "            * (x - d_in[ 8]) / (d_in[ 9] - d_in[ 8]);\n"\
        "    }else if(x < d_in[10]){\n"\
        "        c = d_norm[ 9] + (d_norm[10] - d_norm[ 9])\n"\
        "            * (x - d_in[ 9]) / (d_in[10] - d_in[ 9]);\n"\
        "    }else if(x < d_in[11]){\n"\
        "        c = d_norm[10] + (d_norm[11] - d_norm[10])\n"\
        "            * (x - d_in[10]) / (d_in[11] - d_in[10]);\n"\
        "    }else if(x < d_in[12]){\n"\
        "        c = d_norm[11] + (d_norm[12] - d_norm[11])\n"\
        "            * (x - d_in[11]) / (d_in[12] - d_in[11]);\n"\
        "    }else if(x < d_in[13]){\n"\
        "        c = d_norm[12] + (d_norm[13] - d_norm[12])\n"\
        "            * (x - d_in[12]) / (d_in[13] - d_in[12]);\n"\
        "    }else if(x < d_in[14]){\n"\
        "        c = d_norm[13] + (d_norm[14] - d_norm[13])\n"\
        "            * (x - d_in[13]) / (d_in[14] - d_in[13]);\n"\
        "    }else if(x < d_in[15]){\n"\
        "        c = d_norm[14] + (d_norm[15] - d_norm[14])\n"\
        "            * (x - d_in[14]) / (d_in[15] - d_in[14]);\n"\
        "    }else{\n"\
        "        c = d_norm[num_tbl-1];\n"\
        "    };\n"\
        "    return c;\n"\
        "}\n"\
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
        "    float c = 0.0;\n"\
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
        "float colormap_metal_red(float x) {\n"\
        "    float c = 0.0;\n"\
        "    if (x < 0.0) {\n"\
        "        c = 0.0;\n"\
        "    } else if (x <= 0.6) {\n"\
        "        c = x * 1.666666666666667;\n"\
        "    } else {\n"\
        "        c = 1.0;\n"\
        "    }\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "float colormap_metal_green(float x) {\n"\
        "    float c = 0.0;\n"\
        "    if (x < 0.6) {\n"\
        "        c = 0.0;\n"\
        "    } else if (x <= 1.0) {\n"\
        "        c = (x - 0.6) * 2.5;\n"\
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
        "    float red = 1.0;\n"\
        "\n"\
        "    float c = 0.0;\n"\
        "    if (x < 0.0){\n"\
        "        c = 1.0;\n"\
        "    }else if(x < 0.5){\n"\
        "        c = (1.0 - x * 0.5);\n"\
        "    }else if(x < red){\n"\
        "        c = (red - x) * 1.5;\n"\
        "    }else{\n"\
        "        c = 0.0;\n"\
        "    };\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "float colormap_orangecyan_green(float x) {\n"\
        "    float red = 1.0;\n"\
        "\n"\
        "    float c = 0.0;\n"\
        "    if (x < 0.0){\n"\
        "        c = 1.0;\n"\
        "    }else if(x < 0.5){\n"\
        "        c = 1.0;\n"\
        "    }else if(x < red){\n"\
        "        c = (red - x) + 0.5;\n"\
        "    }else{\n"\
        "        c = 0.5;\n"\
        "    };\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "float colormap_orangecyan_red(float x) {\n"\
        "    float c = 0.0;\n"\
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
        "    float c = 0.0;\n"\
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
        "    float c = 0.0;\n"\
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
        "    float c = 0.0;\n"\
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
        "	float abyss =    0.0;\n"\
        "	float blue =     0.1;\n"\
        "    float white =    0.5;\n"\
        "	float red =      0.9;\n"\
        "	\n"\
        "    float c = 0.0;\n"\
        "	if (x < abyss){\n"\
        "        c = 0.8;\n"\
        "	} else if (x >= abyss && x < blue){\n"\
        "        c = 0.8 + 2.0 * x;\n"\
        "	} else if (x >= blue && x < white){\n"\
        "        c = 1.0 - (x - blue) * 0.25;\n"\
        "	} else if (x >= white && x < red){\n"\
        "        c = (red - x) * 2.0;\n"\
        "	} else {\n"\
        "        c = 0.0;\n"\
        "	}\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "float colormap_redblue_green(float x) {\n"\
        "	float abyss =    0.0;\n"\
        "	float blue =     0.1;\n"\
        "	float white =    0.5;\n"\
        "	float red =      0.9;\n"\
        "	\n"\
        "    float c = 0.0;\n"\
        "	if (x < abyss){\n"\
        "        c = 0.2;\n"\
        "	} else if (x >= abyss && x < blue){\n"\
        "        c = 2.0 * (blue - x);\n"\
        "	} else if (x >= blue && x < white){\n"\
        "        c = (x - blue) * 2.0;\n"\
        "	} else if (x >= white && x < red){\n"\
        "        c = (red - x) * 2.0;\n"\
        "	} else {\n"\
        "        c = 0.0;\n"\
        "	}\n"\
        "    return c;\n"\
        "}\n"\
        "\n"\
        "float colormap_redblue_red(float x) {\n"\
        "	float blue =     0.1;\n"\
        "	float white =    0.5;\n"\
        "	float red =      0.9;\n"\
        "	float blood =    1.0;\n"\
        "	\n"\
        "    float c = 0.0;\n"\
        "	if (x < blue){\n"\
        "        c = 0.0;\n"\
        "	} else if (x >= blue && x < white){\n"\
        "        c = (x - blue) * 2.0;\n"\
        "	} else if (x >= white && x < red){\n"\
        "        c = 1.0 - (red - x) * 0.25;\n"\
        "	} else if (x >= red && x < blood){\n"\
        "        c = 1.0 - (x - red) * 2.0;\n"\
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
        "\n"\
        "struct KemoViewNormalize{\n"\
        "    float data_reference[MAX_NORMALIZATION_POINT];             // Data\n"\
        "    float data_normalized[MAX_NORMALIZATION_POINT];             // normalize\n"\
        "    int num_normalize;\n"\
        "\n"\
        "    float alpha_reference[MAX_NORMALIZATION_POINT];             // Data\n"\
        "    float alpha_output[MAX_NORMALIZATION_POINT];             // normalize\n"\
        "    int num_opacity;\n"\
        "\n"\
        "    int id_cmap;\n"\
        "};\n"\
        "\n"\
        "vec4 colormap_select(int id_cmap, float x, float alpha){\n"\
        "    vec4 c = vec4(0.0, 0.0, 0.0, 0.0);\n"\
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
        "vec4 color_from_scalar(KemoViewNormalize colormap, float x)\n"\
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
        "    float c = 0.0;\n"\
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
        "    float xx = 0.0;\n"\
        "    float c = 0.0;\n"\
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
        "    float c = 0.0;\n"\
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
        "    float c = 0.0;\n"\
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

char * load_dash_lines_3D_frag(void){
    const char  dash_lines_3D_frag_src[]
    = {
        "// -----------------------------------------------------------------------------\n"\
        "// Copyright (c) 2013 Nicolas P. Rougier. All rights reserved.\n"\
        "//\n"\
        "// Redistribution and use in source and binary forms, with or without\n"\
        "// modification, are permitted provided that the following conditions are met:\n"\
        "//\n"\
        "// 1. Redistributions of source code must retain the above copyright notice,\n"\
        "//    this list of conditions and the following disclaimer.\n"\
        "//\n"\
        "// 2. Redistributions in binary form must reproduce the above copyright\n"\
        "//    notice, this list of conditions and the following disclaimer in the\n"\
        "//    documentation and/or other materials provided with the distribution.\n"\
        "//\n"\
        "// THIS SOFTWARE IS PROVIDED BY NICOLAS P. ROUGIER ''AS IS'' AND ANY EXPRESS OR\n"\
        "// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF\n"\
        "// MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO\n"\
        "// EVENT SHALL NICOLAS P. ROUGIER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,\n"\
        "// INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES\n"\
        "// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;\n"\
        "// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND\n"\
        "// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT\n"\
        "// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF\n"\
        "// THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n"\
        "//\n"\
        "// The views and conclusions contained in the software and documentation are\n"\
        "// those of the authors and should not be interpreted as representing official\n"\
        "// policies, either expressed or implied, of Nicolas P. Rougier.\n"\
        "// -----------------------------------------------------------------------------\n"\
        "const float PI = 3.141592653589793;\n"\
        "\n"\
        "\n"\
        "float\n"\
        "cap( float type, float dx, float dy, float t )\n"\
        "{\n"\
        "    float d = 0.0;\n"\
        "\n"\
        "    // None\n"\
        "    if                 ( type < 0.5 )  discard;\n"\
        "    // Round\n"\
        "    else if ( abs(type - 1.0) < 0.5 ) return sqrt(dx*dx+dy*dy);\n"\
        "    // Triangle out\n"\
        "    else if ( abs(type - 2.0) < 0.5 ) return max(abs(dy),(t+dx-abs(dy)));\n"\
        "    // Triangle in\n"\
        "    else if ( abs(type - 3.0) < 0.5 ) return (dx+abs(dy));\n"\
        "    // Square\n"\
        "    else if ( abs(type - 4.0) < 0.5 ) return max(dx,dy);\n"\
        "    // Butt\n"\
        "    else if ( abs(type - 5.0) < 0.5 ) return max(dx+t,dy);\n"\
        "\n"\
        "    discard;\n"\
        "}\n"\
        "\n"\
        "\n"\
        "// Uniforms\n"\
        "// ------------------------------------\n"\
        "uniform mat4      u_view;\n"\
        "uniform mat4      u_projection;\n"\
        "uniform mat4      u_model;\n"\
        "uniform vec2      u_viewport;\n"\
        "uniform sampler2D u_dash_atlas;\n"\
        "\n"\
        "\n"\
        "// Varying\n"\
        "// ------------------------------------\n"\
        "varying vec2  v_texcoord;\n"\
        "varying vec4  v_color;\n"\
        "varying float v_length;\n"\
        "varying float v_linewidth;\n"\
        "varying float v_localwidth;\n"\
        "varying float v_antialias;\n"\
        "varying float v_dash_phase;\n"\
        "varying float v_dash_period;\n"\
        "varying float v_dash_index;\n"\
        "varying vec2  v_dash_caps;\n"\
        "\n"\
        "\n"\
        "void main()\n"\
        "{\n"\
        "    float dx = v_texcoord.x;\n"\
        "    float dy = abs(v_texcoord.y);\n"\
        "    float dash_width = v_linewidth;\n"\
        "    float freq = v_dash_period * dash_width;\n"\
        "    float u = mod( dx + v_dash_phase*dash_width, freq );\n"\
        "    vec4 v = texture2D(u_dash_atlas, vec2(u/freq, v_dash_index));\n"\
        "    float dash_center= v.x * dash_width;\n"\
        "    float dash_type  = v.y;\n"\
        "    float _start = v.z * dash_width;\n"\
        "    float _stop  = v.a * dash_width;\n"\
        "    float dash_start = dx - u + _start;\n"\
        "    float dash_stop  = dx - u + _stop;\n"\
        "    float line_start = 0.0;\n"\
        "    float line_stop = v_length;\n"\
        "\n"\
        "    vec4 color = vec4(v_color.rgb, v_color.a); // * (dx/v_length));\n"\
        "    float t = v_localwidth/2. - 1.5*v_antialias;\n"\
        "    float d = dy;\n"\
        "\n"\
        "\n"\
        "    // Check is dash stop is before line start\n"\
        "    if( dash_stop <= line_start )\n"\
        "    {\n"\
        "        discard;\n"\
        "    }\n"\
        "    // Check is dash start is beyond line stop\n"\
        "    if( dash_start >= line_stop )\n"\
        "    {\n"\
        "        discard;\n"\
        "    }\n"\
        "\n"\
        "    // Line cap start\n"\
        "    if( (dx <= line_start) && (dash_start <= line_start) && (dash_stop >= line_start) )\n"\
        "    {\n"\
        "        float u = v_localwidth * abs(dx) / dash_width;\n"\
        "        d = cap( v_dash_caps.x, u, abs(dy), t);\n"\
        "    }\n"\
        "    // Line stop cap\n"\
        "    else if( (dx >= line_stop) && (dash_stop >= line_stop) && (dash_start <= line_stop) )\n"\
        "    {\n"\
        "        float u = v_localwidth * (dx-line_stop) / dash_width;\n"\
        "        d = cap( v_dash_caps.y, u, abs(dy), t);\n"\
        "    }\n"\
        "    // Dash body (plain)\n"\
        "    else if( dash_type == 0.0 )\n"\
        "    {\n"\
        "        // This is only to save last two tests\n"\
        "    }\n"\
        "    // Dash cap start\n"\
        "    else if( dash_type < 0.0 )\n"\
        "    {\n"\
        "        //float u = v_localwidth * max(u-dash_center, 0.0) / dash_width;\n"\
        "        float u = v_localwidth * max(u-dash_center, 0.0) / dash_width;\n"\
        "        d = cap( v_dash_caps.y, u , abs(dy), t);\n"\
        "    }\n"\
        "    // Dash cap end\n"\
        "    else if( dash_type > 0.0 )\n"\
        "    {\n"\
        "        float u = v_localwidth * max( dash_center-u, 0.0 ) / dash_width;\n"\
        "        d = cap( v_dash_caps.x, u, abs(dy), t);\n"\
        "    }\n"\
        "\n"\
        "\n"\
        "    // Distance to border\n"\
        "    // ------------------------------------------------------------------------\n"\
        "    d = d - t;\n"\
        "    if( d < 0.0 )\n"\
        "    {\n"\
        "        gl_FragColor = color;\n"\
        "    }\n"\
        "    else\n"\
        "    {\n"\
        "        d /= v_antialias;\n"\
        "        float a = exp(-d*d)*color.a;\n"\
        "        if( a < 0.001 )\n"\
        "            discard;\n"\
        "            //gl_FragColor = vec4(1.0,1.0,0.0,1.0);\n"\
        "        else\n"\
        "            gl_FragColor = vec4(color.xyz, a);\n"\
        "    }\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(dash_lines_3D_frag_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, dash_lines_3D_frag_src);
    return src;
};

char * load_dash_lines_3D_vert(void){
    const char  dash_lines_3D_vert_src[]
    = {
        "// -----------------------------------------------------------------------------\n"\
        "// Copyright (c) 2013 Nicolas P. Rougier. All rights reserved.\n"\
        "//\n"\
        "// Redistribution and use in source and binary forms, with or without\n"\
        "// modification, are permitted provided that the following conditions are met:\n"\
        "//\n"\
        "// 1. Redistributions of source code must retain the above copyright notice,\n"\
        "//    this list of conditions and the following disclaimer.\n"\
        "//\n"\
        "// 2. Redistributions in binary form must reproduce the above copyright\n"\
        "//    notice, this list of conditions and the following disclaimer in the\n"\
        "//    documentation and/or other materials provided with the distribution.\n"\
        "//\n"\
        "// THIS SOFTWARE IS PROVIDED BY NICOLAS P. ROUGIER ''AS IS'' AND ANY EXPRESS OR\n"\
        "// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF\n"\
        "// MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO\n"\
        "// EVENT SHALL NICOLAS P. ROUGIER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,\n"\
        "// INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES\n"\
        "// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;\n"\
        "// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND\n"\
        "// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT\n"\
        "// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF\n"\
        "// THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n"\
        "//\n"\
        "// The views and conclusions contained in the software and documentation are\n"\
        "// those of the authors and should not be interpreted as representing official\n"\
        "// policies, either expressed or implied, of Nicolas P. Rougier.\n"\
        "// -----------------------------------------------------------------------------\n"\
        "//\n"\
        "// See http://codeflow.org/entries/2012/aug/05/webgl-rendering-of-solid-trails/\n"\
        "//\n"\
        "// -----------------------------------------------------------------------------\n"\
        "const float PI = 3.141592653589793;\n"\
        "\n"\
        "\n"\
        "// Uniforms\n"\
        "// ------------------------------------\n"\
        "uniform mat4      u_view;\n"\
        "uniform mat4      u_projection;\n"\
        "uniform mat4      u_model;\n"\
        "uniform vec4      u_viewport;\n"\
        "uniform sampler2D u_uniforms;\n"\
        "uniform vec3      u_uniforms_shape;\n"\
        "uniform sampler2D u_dash_atlas;\n"\
        "\n"\
        "\n"\
        "// Attributes\n"\
        "// ------------------------------------\n"\
        "attribute vec3 a_prev;\n"\
        "attribute vec3 a_curr;\n"\
        "attribute vec3 a_next;\n"\
        "attribute vec2 a_texcoord;\n"\
        "attribute float a_index;\n"\
        "\n"\
        "// Varying\n"\
        "// ------------------------------------\n"\
        "varying vec4 v_color;\n"\
        "varying vec2 v_texcoord;\n"\
        "varying float v_length;\n"\
        "varying float v_linewidth;\n"\
        "varying float v_localwidth;\n"\
        "varying float v_antialias;\n"\
        "varying float v_dash_phase;\n"\
        "varying float v_dash_period;\n"\
        "varying float v_dash_index;\n"\
        "varying vec2  v_dash_caps;\n"\
        "\n"\
        "\n"\
        "// Project from the world space to the screen space\n"\
        "vec2 project(vec4 P)\n"\
        "{\n"\
        "    vec2 p = 0.5 + (P.xyz/P.w).xy * 0.5;\n"\
        "    return p * u_viewport.zw;\n"\
        "}\n"\
        "\n"\
        "// Project from the screen space to the world space\n"\
        "vec4 unproject(vec2 p, float z, float w)\n"\
        "{\n"\
        "    vec4 P = vec4( w*((p/u_viewport.zw)*2.0 - 1.0), z, w);\n"\
        "    return P;\n"\
        "}\n"\
        "\n"\
        "// Estimate the linewidth\n"\
        "// WARNING: wrong if position == sPosition\n"\
        "float estimate_width(vec3 position, vec2 sPosition, float width)\n"\
        "{\n"\
        "    vec4 view_pos = u_view * u_model * vec4(position, 1.0);\n"\
        "    vec4 scale_pos = view_pos - vec4(normalize(view_pos.xy)*width, 0.0, 1.0);\n"\
        "    vec2 screen_scale_pos = project(u_projection * scale_pos);\n"\
        "    return distance(sPosition, screen_scale_pos);\n"\
        "}\n"\
        "\n"\
        "void main()\n"\
        "{\n"\
        "    // ------------------------------------------------------- Get uniforms ---\n"\
        "    float rows = u_uniforms_shape.x;\n"\
        "    float cols = u_uniforms_shape.y;\n"\
        "    float count= u_uniforms_shape.z;\n"\
        "    float index = a_index;\n"\
        "    int index_x = int(mod(index, (floor(cols/(count/4.0))))) * int(count/4.0);\n"\
        "    int index_y = int(floor(index / (floor(cols/(count/4.0)))));\n"\
        "    float size_x = cols - 1.0;\n"\
        "    float size_y = rows - 1.0;\n"\
        "    float ty = 0.0;\n"\
        "    if (size_y > 0.0)\n"\
        "        ty = float(index_y)/size_y;\n"\
        "\n"\
        "    int i = index_x;\n"\
        "    vec4 _uniform;\n"\
        "\n"\
        "    // Get fg_color(4)\n"\
        "    v_color = texture2D(u_uniforms, vec2(float(i++)/size_x,ty));\n"\
        "\n"\
        "    // Get v_length(1), v_linewidth(1), v_antialias(1), v_dash_phase(1)\n"\
        "    _uniform = texture2D(u_uniforms, vec2(float(i++)/size_x,ty));\n"\
        "    v_length    = _uniform.x;\n"\
        "    v_linewidth = _uniform.y;\n"\
        "    v_antialias = _uniform.z;\n"\
        "    v_dash_phase= _uniform.w;\n"\
        "\n"\
        "    // Get dash_period(1), dash_index(1), dash_caps(2)\n"\
        "    _uniform = texture2D(u_uniforms, vec2(float(i++)/size_x,ty));\n"\
        "    v_dash_period = _uniform.x;\n"\
        "    v_dash_index  = _uniform.y;\n"\
        "    v_dash_caps.x = _uniform.z;\n"\
        "    v_dash_caps.y = _uniform.w;\n"\
        "    // ------------------------------------------------------------------------\n"\
        "\n"\
        "    mat4 T = u_projection * u_view * u_model;\n"\
        "\n"\
        "    vec2 prev = project( T * vec4(a_prev.xyz,1.0));\n"\
        "    vec2 next = project( T * vec4(a_next.xyz,1.0));\n"\
        "    vec4 tcurr =         T * vec4(a_curr.xyz,1.0);\n"\
        "    vec2 curr = project(tcurr);\n"\
        "\n"\
        "    vec2 tangent1 = normalize(prev - curr);\n"\
        "    vec2 tangent2 = normalize(curr - next);\n"\
        "    vec2 tangent = normalize(tangent1 + tangent2);\n"\
        "    vec2 ortho = vec2(-tangent.y, tangent.x)*a_texcoord.y;\n"\
        "    v_localwidth = estimate_width(a_curr.xyz, curr, v_linewidth);\n"\
        "    float w = v_localwidth/2. + 1.25*v_antialias;\n"\
        "\n"\
        "    vec2 pos = curr + w*ortho;\n"\
        "    v_texcoord.x = a_texcoord.x;\n"\
        "    v_texcoord.y = a_texcoord.y * w;\n"\
        "\n"\
        "    if( v_texcoord.x <= 0.0 )\n"\
        "    {\n"\
        "        pos += w*tangent;\n"\
        "        v_texcoord.x -= v_linewidth/2.;\n"\
        "    }\n"\
        "    else if( v_texcoord.x >= v_length )\n"\
        "    {\n"\
        "        pos -= w*tangent;\n"\
        "        v_texcoord.x += v_linewidth/2.;\n"\
        "    }\n"\
        "\n"\
        "    gl_Position = unproject(pos, tcurr.z, tcurr.w);\n"\
        "    //gl_Position = unproject(pos, v_position.z, v_position.w);\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(dash_lines_3D_vert_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, dash_lines_3D_vert_src);
    return src;
};

char * load_gouraud_frag(void){
    const char  gouraud_frag_src[]
    = {
        "// #version 330\n"\
        "//\n"\
        "// gouraud.frag\n"\
        "//\n"\
        "\n"\
        "in vec4 ex_Position;\n"\
        "in vec4 ex_Color;\n"\
        "\n"\
        "out vec4 out_Color;\n"\
        "\n"\
        "void main (void)\n"\
        "{\n"\
        "	out_Color = ex_Color;\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(gouraud_frag_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, gouraud_frag_src);
    return src;
};

char * load_gouraud_vert(void){
    const char  gouraud_vert_src[]
    = {
        "//#version 330\n"\
        "//\n"\
        "// gouraud.vert\n"\
        "//\n"\
        "\n"\
        "layout (location = 0) in vec4  xyz;\n"\
        "layout (location = 1) in vec4  color;\n"\
        "layout (location = 2) in vec4  norm;\n"\
        "layout (location = 3) in vec2  txur;\n"\
        "layout (location = 4) in vec2  data;\n"\
        "\n"\
        "out vec4 ex_Position;\n"\
        "out vec4 ex_Color;\n"\
        "\n"\
        "uniform mat4 projectionMat;\n"\
        "uniform mat4 viewMatrix;\n"\
        "uniform mat4 modelViewMat;\n"\
        "uniform mat4 modelNormalMat;\n"\
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
        "\n"\
        "void main(void)\n"\
        "{\n"\
        "    vec4 position.xyz = xyz;\n"\
        "    position.w = 1.0;\n"\
        "    position = modelViewMat * position;\n"\
        "\n"\
        "	vec4 norm4 =  modelNormalMat * norm;\n"\
        "    vec3 normal = normalize(norm4.xyz);\n"\
        "	vec3 light;\n"\
        "	float diffuse;\n"\
        "	\n"\
        "	ex_Color = vec4(0.0,0.0,0.0,0.0);\n"\
        "	for (int i = 0; i < num_lights; ++i){\n"\
        "		light = normalize(LightSource[0].position.xyz - position.xyz);\n"\
        "		diffuse = dot(light, normal);\n"\
        "		\n"\
        "		ex_Color += frontMaterial.ambient;\n"\
        "		if (diffuse > 0.0) {\n"\
        "			vec3 view = normalize(position.xyz);\n"\
        "			vec3 halfway = normalize(light - view);\n"\
        "			float specular = pow(max(dot(normal, halfway), 0.0), frontMaterial.shininess);\n"\
        "			ex_Color += frontMaterial.diffuse * diffuse\n"\
        "						  + frontMaterial.specular * specular;\n"\
        "		}\n"\
        "	}\n"\
        "	\n"\
        "	ex_Position =  projectionMat * position;\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(gouraud_vert_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, gouraud_vert_src);
    return src;
};

char * load_menu_frag(void){
    const char  menu_frag_src[]
    = {
        "// #version 330\n"\
        "//\n"\
        "//   menu.flag\n"\
        "//\n"\
        "\n"\
        "\n"\
        "in vec4 ex_Color;\n"\
        "out vec4 out_Color;\n"\
        "\n"\
        "void main(void)\n"\
        "{\n"\
        "    out_Color = ex_Color;\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(menu_frag_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, menu_frag_src);
    return src;
};

char * load_menu_vert(void){
    const char  menu_vert_src[]
    = {
        "// #version 330\n"\
        "//\n"\
        "//  menu.vert\n"\
        "//\n"\
        "\n"\
        "layout(location=0) in vec4 in_Position;\n"\
        "layout(location=1) in vec4 in_Color;\n"\
        "out vec4 ex_Color;\n"\
        "\n"\
        "void main(void)\n"\
        "{\n"\
        "    gl_Position = vec4(in_Position.xyz, 1.0);\n"\
        "    ex_Color = in_Color;\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(menu_vert_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, menu_vert_src);
    return src;
};

char * load_phong_frag(void){
    const char  phong_frag_src[]
    = {
        "// #version 330\n"\
        "//\n"\
        "// phong.frag\n"\
        "//\n"\
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
    
    long n = strlen(phong_frag_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, phong_frag_src);
    return src;
};

char * load_phong_vert(void){
    const char  phong_vert_src[]
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
        "\n"\
        "out vec4 position;\n"\
        "out vec4 ex_Color;\n"\
        "out vec4 normal;\n"\
        "\n"\
        "void main(void)\n"\
        "{\n"\
        "    position = xyz;\n"\
        "    position.w = 1.0;\n"\
        "    position = modelViewMat * position;\n"\
        "	normal =   modelNormalMat * norm;\n"\
        "	ex_Color = color;\n"\
        "\n"\
        "	gl_Position =  projectionMat * position;\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(phong_vert_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, phong_vert_src);
    return src;
};

char * load_phong_1color_frag(void){
    const char  phong_1color_frag_src[]
    = {
        "// #version 330\n"\
        "//\n"\
        "//\n"\
        "// phong_1color.frag\n"\
        "//\n"\
        "//\n"\
        "\n"\
        "in vec4 position;\n"\
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
        "uniform vec4 SingleColor;\n"\
        "\n"\
        "void main (void)\n"\
        "{\n"\
        "	vec3 fnormal = normalize(normal.xyz);\n"\
        "	vec3 light;\n"\
        "	float diffuse;\n"\
        "	\n"\
        "    vec3 halfway;\n"\
        "    float product;\n"\
        "    float fspecular;\n"\
        "\n"\
        "    vec3 view = normalize(position.xyz);\n"\
        "    vec4 tmpsp =  vec4(frontMaterial.specular.xyz, ex_Color.w);\n"\
        "\n"\
        "    out_Color = vec4(0.0,0.0,0.0,0.0);\n"\
        "	for (int i = 0; i < num_lights; ++i){\n"\
        "		light = normalize(LightSource[i].position.xyz - position.xyz);\n"\
        "\n"\
        "        halfway = normalize(light - view);\n"\
        "        product = max(dot(fnormal, halfway), 0.0);\n"\
        "        fspecular = pow(product, frontMaterial.shininess);\n"\
        "		diffuse = dot(light, fnormal);\n"\
        "		\n"\
        "		out_Color += SingleColor * frontMaterial.ambient\n"\
        "			        + SingleColor * frontMaterial.diffuse * abs(diffuse)\n"\
        "                    + tmpsp * fspecular;\n"\
        "	}\n"\
        "}\n"\
        "\n"\
        "\n"
    };
    
    long n = strlen(phong_1color_frag_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, phong_1color_frag_src);
    return src;
};

char * load_phong_1color_vert(void){
    const char  phong_1color_vert_src[]
    = {
        "// #version 330\n"\
        "//\n"\
        "// phong_1color.vert\n"\
        "//\n"\
        "\n"\
        "layout(location = 0) in vec4  xyz;\n"\
        "layout(location = 1) in vec4  norm;\n"\
        "\n"\
        "\n"\
        "uniform mat4 projectionMat;\n"\
        "uniform mat4 viewMatrix;\n"\
        "uniform mat4 modelViewMat;\n"\
        "uniform mat4 modelNormalMat;\n"\
        "\n"\
        "out vec4 position;\n"\
        "out vec4 normal;\n"\
        "\n"\
        "void main(void)\n"\
        "{\n"\
        "    vec4 position = xyz;\n"\
        "    position.w = 1.0;\n"\
        "    position = modelViewMat * position;\n"\
        "\n"\
        "	normal = modelNormalMat * norm;\n"\
        "	\n"\
        "	gl_Position =  projectionMat * position;\n"\
        "}\n"\
        "\n"\
        "\n"
    };
    
    long n = strlen(phong_1color_vert_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, phong_1color_vert_src);
    return src;
};

char * load_phong_texture_frag(void){
    const char  phong_texture_frag_src[]
    = {
        "// #version 330\n"\
        "//\n"\
        "// phong_texture.frag\n"\
        "//\n"\
        "\n"\
        "in vec4 position;\n"\
        "in vec4 ex_Color;\n"\
        "in vec4 normal;\n"\
        "in vec2 tex_position;\n"\
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
        "uniform sampler2D image;\n"\
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
        "	vec4 txColor = texture(image, tex_position);\n"\
        "    vec4 addColor = vec4(txColor.xyz, ex_Color.w);\n"\
        "\n"\
        "    vec3 out_Color3;\n"\
        "    float opacity;\n"\
        "    out_Color = vec4(0.0,0.0,0.0,0.0);\n"\
        "	for (int i = 0; i < num_lights; ++i){\n"\
        "		light = normalize(LightSource[i].position.xyz - position.xyz);\n"\
        "        halfway = normalize(light - view);\n"\
        "        product = max(dot(fnormal, halfway), 0.0);\n"\
        "        fspecular = pow(product, frontMaterial.shininess);\n"\
        "\n"\
        "        diffuse = dot(light, fnormal);\n"\
        "\n"\
        "        out_Color += addColor * frontMaterial.ambient;\n"\
        "                   + addColor * frontMaterial.diffuse * abs(diffuse)\n"\
        "                   + tmpsp * fspecular;\n"\
        "	}\n"\
        "}\n"\
        "\n"\
        "\n"
    };
    
    long n = strlen(phong_texture_frag_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, phong_texture_frag_src);
    return src;
};

char * load_phong_texture_vert(void){
    const char  phong_texture_vert_src[]
    = {
        "// #version 330\n"\
        "//\n"\
        "// phong_texture.vert\n"\
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
        "\n"\
        "out vec4 position;\n"\
        "out vec4 ex_Color;\n"\
        "out vec4 normal;\n"\
        "out vec2 tex_position;\n"\
        "\n"\
        "void main(void)\n"\
        "{\n"\
        "    position = xyz;\n"\
        "    position.w = 1.0;\n"\
        "    position = modelViewMat * position;\n"\
        "\n"\
        "    normal = modelNormalMat * norm;\n"\
        "	ex_Color = color;\n"\
        "	tex_position = txur;\n"\
        "\n"\
        "	gl_Position =  projectionMat * position;\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(phong_texture_vert_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, phong_texture_vert_src);
    return src;
};

char * load_phong_w_colormap_frag(void){
    const char  phong_w_colormap_frag_src[]
    = {
        "// #version 330\n"\
        "//\n"\
        "// phong.frag\n"\
        "//\n"\
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
        "	gl_Position =  projectionMat * position;\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(phong_w_colormap_vert_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, phong_w_colormap_vert_src);
    return src;
};

char * load_simple_frag(void){
    const char  simple_frag_src[]
    = {
        "// #version 330\n"\
        "//\n"\
        "// simple.frag\n"\
        "//\n"\
        "\n"\
        "in vec4 ex_Color;\n"\
        "out vec4 out_Color;\n"\
        "\n"\
        "void main(){\n"\
        "    out_Color = ex_Color;\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(simple_frag_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, simple_frag_src);
    return src;
};

char * load_simple_vert(void){
    const char  simple_vert_src[]
    = {
        "// #version 330\n"\
        "//\n"\
        "// simple.vert\n"\
        "//\n"\
        "\n"\
        "layout (location = 0) in vec4  xyz;\n"\
        "layout (location = 1) in vec4  color;\n"\
        "\n"\
        "out vec4 ex_Color;\n"\
        "\n"\
        "uniform mat4 projectionMat;\n"\
        "uniform mat4 viewMatrix;\n"\
        "uniform mat4 modelViewMat;\n"\
        "\n"\
        "\n"\
        "void main(void)\n"\
        "{\n"\
        "    vec4 position = xyz;\n"\
        "    position.w = 1.0;\n"\
        "    position = modelViewMat * position;\n"\
        "	gl_Position = projectionMat * position;\n"\
        "	ex_Color = color;\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(simple_vert_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, simple_vert_src);
    return src;
};

char * load_simple_texture_frag(void){
    const char  simple_texture_frag_src[]
    = {
        "// #version 330\n"\
        "//\n"\
        "// simple_texture.frag\n"\
        "//\n"\
        "\n"\
        "in vec2 tex_position;\n"\
        "\n"\
        "out vec4 out_Color;\n"\
        "\n"\
        "uniform sampler2D image;\n"\
        "\n"\
        "void main (void)\n"\
        "{\n"\
        "	out_Color = texture(image, tex_position);\n"\
        "}\n"\
        "\n"\
        "\n"
    };
    
    long n = strlen(simple_texture_frag_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, simple_texture_frag_src);
    return src;
};

char * load_simple_texture_vert(void){
    const char  simple_texture_vert_src[]
    = {
        "// #version 330\n"\
        "//\n"\
        "// simple_texture.vert\n"\
        "//\n"\
        "\n"\
        "layout (location = 0) in vec4  xyz;\n"\
        "layout (location = 1) in vec2  txur;\n"\
        "\n"\
        "\n"\
        "uniform mat4 projectionMat;\n"\
        "uniform mat4 modelViewMat;\n"\
        "\n"\
        "out vec2 tex_position;\n"\
        "\n"\
        "void main(void)\n"\
        "{\n"\
        "    vec4 position = xyz;\n"\
        "    position.w = 1.0;\n"\
        "	position = modelViewMat * position;\n"\
        "	tex_position = txur;\n"\
        "\n"\
        "	gl_Position =  projectionMat * position;\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(simple_texture_vert_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, simple_texture_vert_src);
    return src;
};

