#version 330
// phong_texture.vert

layout (location = 0) in vec3  xyz;
layout (location = 1) in float data;
layout (location = 2) in vec4  color;
layout (location = 3) in vec4  norm;
layout (location = 4) in vec2  txur;


uniform mat4 projectionMat;
uniform mat4 viewMatrix;
uniform mat4 modelViewMat;
uniform mat4 modelNormalMat;

out vec4 position;
out vec4 ex_Color;
out vec4 normal;
out vec2 tex_position;

void main(void)
{
	position = vec4(modelViewMat * vec4(xyz, 1.0));
	normal = modelNormalMat * norm;
	ex_Color = color;
	tex_position = txur;

	gl_Position =  projectionMat * position;
}
