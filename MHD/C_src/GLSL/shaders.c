/*
//  Source of shader texts
//  Generated from shader files
*/

#include "shaders.h"

char * load_gouraud_frag(){
    const char  gouraud_frag[]
    = {
        "#version 330 core\n"\
        "// gouraud.frag\n"\
        "\n"\
        "void main (void)\n"\
        "{\n"\
        "	gl_FragColor = gl_Color;\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(gouraud_frag);
    char * src = alloc_string((int) n+1);
    
    strcpy(src, gouraud_frag);
    return src;
};

char * load_gouraud_vert(){
    const char  gouraud_vert[]
    = {
        "#version 330 core\n"\
        "// gouraud.vert\n"\
        "\n"\
        "uniform mat4 projectionMatrix;\n"\
        "uniform mat4 viewMatrix;\n"\
        "uniform mat4 modelViewMatrix;\n"\
        "\n"\
        "void main(void)\n"\
        "{\n"\
        "	vec3 position = vec3(modelViewMatrix * gl_Vertex);\n"\
        "	vec3 normal = normalize(gl_NormalMatrix * gl_Normal);\n"\
        "	vec3 light = normalize(gl_LightSource[0].position.xyz - position);\n"\
        "	float diffuse = dot(light, normal);\n"\
        "	\n"\
        "	gl_FrontColor = gl_FrontLightProduct[0].ambient;\n"\
        "	if (diffuse > 0.0) {\n"\
        "		vec3 view = normalize(position);\n"\
        "		vec3 halfway = normalize(light - view);\n"\
        "		float specular = pow(max(dot(normal, halfway), 0.0), gl_FrontMaterial.shininess);\n"\
        "		gl_FrontColor += gl_FrontLightProduct[0].diffuse * diffuse\n"\
        "					  + gl_FrontLightProduct[0].specular * specular;\n"\
        "	}\n"\
        "	\n"\
        "	gl_Position =  projectionMatrix * vec4(position, 1.0);\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(gouraud_vert);
    char * src = alloc_string((int) n+1);
    
    strcpy(src, gouraud_vert);
    return src;
};

char * load_menu_frag(){
    const char  menu_frag[]
    = {
        "#version 400\n"\
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
    
    long n = strlen(menu_frag);
    char * src = alloc_string((int) n+1);
    
    strcpy(src, menu_frag);
    return src;
};

char * load_menu_vert(){
    const char  menu_vert[]
    = {
        "#version 400\n"\
        "\n"\
        "layout(location=0) in vec4 in_Position;\n"\
        "layout(location=1) in vec4 in_Color;\n"\
        "out vec4 ex_Color;\n"\
        "\n"\
        "void main(void)\n"\
        "{\n"\
        "    gl_Position = in_Position;\n"\
        "    ex_Color = in_Color;\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(menu_vert);
    char * src = alloc_string((int) n+1);
    
    strcpy(src, menu_vert);
    return src;
};

char * load_phong_frag(){
    const char  phong_frag[]
    = {
        "#version 330 core\n"\
        "// phong.frag\n"\
        "\n"\
        "out vec3 position;\n"\
        "out vec3 normal;\n"\
        "\n"\
        "void main (void)\n"\
        "{\n"\
        "	vec3 fnormal = normalize(normal);\n"\
        "	vec3 light = normalize(gl_LightSource[0].position.xyz - position);\n"\
        "	float diffuse = dot(light, fnormal);\n"\
        "	\n"\
        "	gl_FragColor = gl_FrontLightProduct[0].ambient;\n"\
        "	if (diffuse > 0.0) {\n"\
        "		vec3 view = normalize(position);\n"\
        "		vec3 halfway = normalize(light - view);\n"\
        "		float specular = pow(max(dot(fnormal, halfway), 0.0), gl_FrontMaterial.shininess);\n"\
        "		gl_FragColor += gl_FrontLightProduct[0].diffuse * diffuse\n"\
        "		              + gl_FrontLightProduct[0].specular * specular;\n"\
        "	}\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(phong_frag);
    char * src = alloc_string((int) n+1);
    
    strcpy(src, phong_frag);
    return src;
};

char * load_phong_vert(){
    const char  phong_vert[]
    = {
        "#version 330 core\n"\
        "// phong.vert\n"\
        "\n"\
        "uniform mat4 projectionMatrix;\n"\
        "uniform mat4 viewMatrix;\n"\
        "uniform mat4 modelViewMatrix;\n"\
        "\n"\
        "out vec3 position;\n"\
        "out vec3 normal;\n"\
        "\n"\
        "void main(void)\n"\
        "{\n"\
        "	position = vec3(modelViewMatrix * gl_Vertex);\n"\
        "	normal = normalize(gl_NormalMatrix * gl_Normal);\n"\
        "\n"\
        "	gl_Position =  projectionMatrix * vec4(position, 1.0);\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(phong_vert);
    char * src = alloc_string((int) n+1);
    
    strcpy(src, phong_vert);
    return src;
};

char * load_test_frag(){
    const char  test_frag[]
    = {
        "#version 330 core\n"\
        "out vec3 color;\n"\
        "\n"\
        "void main(){\n"\
        "    color = vec3(1,0,0);\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(test_frag);
    char * src = alloc_string((int) n+1);
    
    strcpy(src, test_frag);
    return src;
};

char * load_test_vert(){
    const char  test_vert[]
    = {
        "#version 330 core\n"\
        "// test.vert\n"\
        "\n"\
        "layout(location = 0) in vec3 vertexPosition_modelspace;\n"\
        "\n"\
        "uniform mat4 projectionMatrix;\n"\
        "uniform mat4 viewMatrix;\n"\
        "uniform mat4 modelViewMatrix;\n"\
        "\n"\
        "void main(void)\n"\
        "{\n"\
        "    gl_Position.xyz = vertexPosition_modelspace;\n"\
        "    gl_Position.w = 1.0;\n"\
        "}\n"\
        "\n"
    };
    
    long n = strlen(test_vert);
    char * src = alloc_string((int) n+1);
    
    strcpy(src, test_vert);
    return src;
};

