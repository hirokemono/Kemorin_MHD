#version 330
// phong.vert

layout (location = 0) in vec3  xyz;
layout (location = 1) in float data;
layout (location = 2) in vec4  color;
layout (location = 3) in vec3  norm;
layout (location = 4) in vec2  txur;


uniform mat4 projectionMat;
uniform mat4 viewMatrix;
uniform mat4 modelViewMat;
uniform mat3 modelNormalMat;

out vec4 position;
out vec4 ex_Color;
out vec3 normal;

void main(void)
{
	position = vec4(modelViewMat * vec4(xyz, 1.0));
	normal = normalize(modelNormalMat * norm);
	ex_Color = color;

	gl_Position =  projectionMat * position;
}
