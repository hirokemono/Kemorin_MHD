//
//  gouraud.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 13/02/05.
//
//

#include "gouraud.h"

void SetGouraudShaderSrc(GLuint GouraudGl2Program)
{
	const GLchar *gouraud_vsrc =
    "// gouraud.vert\n"
    "\n"
    "void main(void)\n"
    "{\n"
    "	vec3 position = vec3(gl_ModelViewMatrix * gl_Vertex);\n"
    "	vec3 normal = normalize(gl_NormalMatrix * gl_Normal);\n"
    "	vec3 light = normalize(gl_LightSource[0].position.xyz - position);\n"
    "	float diffuse = dot(light, normal);\n"
    "	\n"
    "	gl_FrontColor = gl_FrontLightProduct[0].ambient;\n"
    "	if (diffuse > 0.0) {\n"
    "		vec3 view = normalize(position);\n"
    "		vec3 halfway = normalize(light - view);\n"
    "		float specular = pow(max(dot(normal, halfway), 0.0), gl_FrontMaterial.shininess);\n"
    "		gl_FrontColor += gl_FrontLightProduct[0].diffuse * diffuse\n"
    "					  + gl_FrontLightProduct[0].specular * specular;\n"
    "	}\n"
    "\n"
    "	gl_Position =  gl_ProjectionMatrix * vec4(position, 1.0);\n"
    "}\n";
	
	const GLchar *gouraud_fsrc =
    "// gouraud.frag\n"
    "\n"
    "void main (void)\n"
    "{\n"
    "	gl_FragColor = gl_Color;\n"
    "}\n";
	
	/* Compile and link  shader */
	LoadShaderFromStrings(&GouraudGl2Program, gouraud_vsrc, gouraud_fsrc);
	
	return;
}
