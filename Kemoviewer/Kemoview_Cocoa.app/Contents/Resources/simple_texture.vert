#version 330
// simple_texture.vert

layout (location = 0) in vec3  xyz;
layout (location = 1) in vec2  txur;


uniform mat4 projectionMat;
uniform mat4 modelViewMat;

out vec4 position;
out vec2 tex_position;

void main(void)
{
	position = vec4(modelViewMat * vec4(xyz, 1.0));
	tex_position = txur;

	gl_Position =  projectionMat * position;
}
