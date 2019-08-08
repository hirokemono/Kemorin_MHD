#version 330 core
// test.vert

layout(location = 0) in vec3 vertexPosition_modelspace;

uniform mat4 projectionMatrix;
uniform mat4 viewMatrix;
uniform mat4 modelViewMatrix;

void main(void)
{
    gl_Position.xyz = vertexPosition_modelspace;
    gl_Position.w = 1.0;
}
