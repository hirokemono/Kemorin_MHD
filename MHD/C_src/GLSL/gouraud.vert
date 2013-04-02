// gouraud.vert

void main(void)
{
	vec3 position = vec3(gl_ModelViewMatrix * gl_Vertex);
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
	
	gl_Position =  gl_ProjectionMatrix * vec4(position, 1.0);
}
