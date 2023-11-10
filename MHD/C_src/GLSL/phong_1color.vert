#version 330
// phong_1color.vert

layout(location = 0) in vec3  xyz;
layout(location = 1) in vec4  norm;


uniform mat4 projectionMat;
uniform mat4 viewMatrix;
uniform mat4 modelViewMat;
uniform mat4 modelNormalMat;

out vec4 position;
out vec4 normal;

void main(void)
{
	position = vec4(modelViewMat * vec4(xyz, 1.0));
	normal = modelNormalMat * norm;
	
	gl_Position =  projectionMat * position;
}

