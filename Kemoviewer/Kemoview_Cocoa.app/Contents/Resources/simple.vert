#version 330
// simple.vert

layout (location = 0) in vec3  xyz;
// layout (location = 1) in vec3  norm;
// layout (location = 2) in vec2  txur;
layout (location = 1) in vec4  color;

out vec4 ex_Color;

uniform mat4 projectionMat;
uniform mat4 viewMatrix;
uniform mat4 modelViewMat;


void main(void)
{
	gl_Position = vec4(projectionMat *  modelViewMat * vec4(xyz, 1.0));
	ex_Color = color;
}
