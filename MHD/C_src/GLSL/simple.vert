#version 330
// simple.vert

layout (location = 0) in vec4  xyz;
layout (location = 1) in vec4  color;

out vec4 ex_Color;

uniform mat4 projectionMat;
uniform mat4 viewMatrix;
uniform mat4 modelViewMat;


void main(void)
{
    vec4 position = xyz;
    position.w = 1.0;
    position = modelViewMat * position;
	gl_Position = projectionMat * position;
	ex_Color = color;
}
