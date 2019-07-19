//
//  phong.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 13/02/05.
//
//


#include "phong.h"

void SetPhongShaderSrc(GLuint PhongGl2Program)
{
	const GLchar *phong_vsrc =
    "// phong.vert\n"
	"#version 330\n"
	"\n"
    "varying vec3 position;\n"
    "varying vec3 normal;\n"
    "void main(void)\n"
    "{\n"
    "	position = vec3(gl_ModelViewMatrix * gl_Vertex);\n"
    "	normal = normalize(gl_NormalMatrix * gl_Normal);\n"
    "	gl_Position =  gl_ProjectionMatrix * vec4(position, 1.0);\n"
    "}\n";
	
	const GLchar *phong_fsrc =
    "// phong.frag\n"
	"#version 330\n"
    "\n"
    "varying vec3 position;\n"
    "varying vec3 normal;\n"
    "\n"
    "void main (void)\n"
    "{\n"
    "	vec3 fnormal = normalize(normal);\n"
    "	vec3 light = normalize(gl_LightSource[0].position.xyz - position);\n"
    "	float diffuse = dot(light, fnormal);\n"
    "	\n"
    "	gl_FragColor = gl_FrontLightProduct[0].ambient;\n"
    "	if (diffuse > 0.0) {\n"
    "		vec3 view = normalize(position);\n"
    "		vec3 halfway = normalize(light - view);\n"
    "		float specular = pow(max(dot(fnormal, halfway), 0.0), gl_FrontMaterial.shininess);\n"
    "		gl_FragColor += gl_FrontLightProduct[0].diffuse * diffuse\n"
    "		              + gl_FrontLightProduct[0].specular * specular;\n"
    "	}\n"
    "}\n";
	
	struct file_text *phong_vertex = init_file_text("/Users/matsui/src_kemo/MHD/C_src/GLSL/phong.vert");
	struct file_text *phong_fragment = init_file_text("/Users/matsui/src_kemo/MHD/C_src/GLSL/phong.frag");
	
	read_text_to_carray(phong_vertex);
	read_text_to_carray(phong_fragment);
/*	
	check_file_contents(phong_vertex);
	check_file_contents(phong_fragment);
*/	
	/* Compile and link  shader */
	LoadShaderFromStrings(&PhongGl2Program, phong_vertex->text, phong_fragment->text);
//	LoadShaderFromStrings(&PhongGl2Program, phong_vsrc, phong_fsrc);
//	LoadShaderFromStrings(&PhongGl2Program, phong_vsrc, phong_fsrc);
	return;
}
