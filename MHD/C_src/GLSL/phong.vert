#version 400
// phong.vert

layout (location = 0) in vec3  xyz;
layout (location = 1) in float data;
layout (location = 2) in vec4  color;
layout (location = 3) in vec3  norm;
layout (location = 4) in vec2  txur;


uniform mat4 projectionMat;
uniform mat4 viewMatrix;
uniform mat4 modelViewMat;

out vec3 position;
out vec3 normal;

void main(void)
{
	position = vec3(modelViewMat * gl_Vertex);
	normal = normalize(gl_NormalMatrix * gl_Normal);

	gl_Position =  projectionMat * vec4(position, 1.0);
}
