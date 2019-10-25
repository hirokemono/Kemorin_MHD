#version 330
// gouraud.vert

layout (location = 0) in vec3  xyz;
layout (location = 1) in float data;
layout (location = 2) in vec4  color;
layout (location = 3) in vec3  norm;
layout (location = 4) in vec2  txur;

out vec4 ex_Position;
out vec4 ex_Color;

uniform mat4 projectionMat;
uniform mat4 viewMatrix;
uniform mat4 modelViewMat;
uniform mat3 modelNormalMat;

#define MAX_LIGHTS 10
struct LightSourceParameters{   
	vec4 ambient;              // Aclarri   
	vec4 diffuse;              // Dcli   
	vec4 specular;             // Scli   
	vec4 position;             // Ppli   
	vec4 halfVector;           // Derived: Hi   
	vec3 spotDirection;        // Sdli   
	float spotExponent;        // Srli   
	float spotCutoff;          // Crli                              
	// (range: [0.0,90.0], 180.0)   
	float spotCosCutoff;       // Derived: cos(Crli)                 
	// (range: [1.0,0.0],-1.0)   
	float constantAttenuation;   // K0   
	float linearAttenuation;     // K1   
	float quadraticAttenuation;  // K2  
};    
uniform int num_lights;
uniform LightSourceParameters LightSource[MAX_LIGHTS];

struct ColorMaterial {
	vec4 emission;    // Ecm   
	vec4 ambient;     // Acm   
	vec4 diffuse;     // Dcm   
	vec4 specular;    // Scm   
	float shininess;  // Srm  
};
uniform ColorMaterial frontMaterial;
uniform ColorMaterial backMaterial;


void main(void)
{
	vec3 position = vec3(modelViewMat * vec4(xyz, 1.0));
	vec3 normal = normalize(modelNormalMat * norm);
	vec3 light;
	float diffuse;
	
	ex_Color = vec4(0.0,0.0,0.0,0.0);
	for (int i = 0; i < num_lights; ++i){
		light = normalize(LightSource[0].position.xyz - position);
		diffuse = dot(light, normal);
		
		ex_Color += frontMaterial.ambient;
		if (diffuse > 0.0) {
			vec3 view = normalize(position);
			vec3 halfway = normalize(light - view);
			float specular = pow(max(dot(normal, halfway), 0.0), frontMaterial.shininess);
			ex_Color += frontMaterial.diffuse * diffuse
						  + frontMaterial.specular * specular;
		}
	}
	
	ex_Position =  projectionMat * vec4(position, 1.0);
}
