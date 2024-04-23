/*
//  Source of shader texts
//  Generated from shader files
*/

#include "test_shaders.h"

char * load_phong_cmap_frag(void){
    const char  phong_cmap_frag_src[]
    = {
        "\n"\
        "//\n"\
        "// phong.frag\n"\
        "//\n"\
        "#version 330\n"\
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
        "    vec4 new_color = darkcolor(ex_Color);\n"\
        "\n"\
        "    vec3 view =   normalize(position.xyz);\n"\
        "    vec4 tmpsp =  vec4(frontMaterial.specular.xyz, new_color.w);\n"\
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
        "		out_Color += new_color * frontMaterial.ambient\n"\
        "                    + new_color * frontMaterial.diffuse * abs(diffuse)\n"\
        "                    + tmpsp * fspecular;\n"\
        "\n"\
        "    }\n"\
        "}\n"\
        "\n"\
        "vec4 darkcolor(vec4 x)\n"\
        "{\n"\
        "    return x * 0.2;\n"\
        "}\n"\
        "\n"\
        "\n"\
        "\n"
    };
    
    long n = strlen(phong_cmap_frag_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, phong_cmap_frag_src);
    return src;
};

char * load_phong_cmap_vert(void){
    const char  phong_cmap_vert_src[]
    = {
        "//\n"\
        "// phong.vert\n"\
        "//\n"\
        "\n"\
        "#version 330\n"\
        "\n"\
        "layout (location = 0) in vec4  xyz;\n"\
        "layout (location = 1) in vec4  color;\n"\
        "layout (location = 2) in vec4  norm;\n"\
        "layout (location = 3) in vec2  txur;\n"\
        "layout (location = 4) in float data;\n"\
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
    
    long n = strlen(phong_cmap_vert_src);
    char * src = alloc_string((long) n+1);
    
    strcpy(src, phong_cmap_vert_src);
    return src;
};

