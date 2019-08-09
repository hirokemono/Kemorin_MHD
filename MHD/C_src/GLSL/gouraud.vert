#version 400
// gouraud.vert

layout (location = 0) in vec3  xyz;
layout (location = 1) in float data;
layout (location = 2) in vec4  color;
layout (location = 3) in vec3  norm;
layout (location = 4) in vec2  txur;

uniform mat4 projectionMat;
uniform mat4 viewMatrix;
uniform mat4 modelViewMat;

void main(void)
{
	vec3 position = vec3(modelViewMat * gl_Vertex);
	vec3 normal = normalize(gl_NormalMatrix * gl_Normal);
	vec3 light = normalize(gl_LightSource[0].position.xyz - position);
	float diffuse = dot(light, normal);
	
	gl_FrontColor = gl_FrontLightProduct[0].ambient;
	if (diffuse > 0.0) {
		vec3 view = normalize(position);
		vec3 halfway = normalize(light - view);
		float specular = pow(max(dot(normal, halfway), 0.0), gl_FrontMaterial.shininess);
		gl_FrontColor += gl_FrontLightProduct[0].diffuse * diffuse
					  + gl_FrontLightProduct[0].specular * specular;
	}
	
	gl_Position =  projectionMat * vec4(position, 1.0);
}
