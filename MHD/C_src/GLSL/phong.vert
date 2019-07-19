#version 330 core
// phong.vert

uniform mat4 projectionMatrix;
uniform mat4 viewMatrix;
uniform mat4 modelViewMatrix;

out vec3 position;
out vec3 normal;

void main(void)
{
	position = vec3(modelViewMatrix * gl_Vertex);
	normal = normalize(gl_NormalMatrix * gl_Normal);

	gl_Position =  projectionMatrix * vec4(position, 1.0);
}
