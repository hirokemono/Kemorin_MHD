// #version 330
//
// phong.frag
//

in vec4 position;
in vec4 ex_Color;
in vec4 normal;
out vec4 out_Color;

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

void main (void)
{
	vec3 fnormal = normalize(normal.xyz);
	vec3 light;
	float diffuse;

    vec3 halfway;
    float product;
    float fspecular;
    
    vec3 view =   normalize(position.xyz);
    vec4 tmpsp =  vec4(frontMaterial.specular.xyz, ex_Color.w);

	out_Color = vec4(0.0,0.0,0.0,0.0);
	for (int i = 0; i < num_lights; ++i){
		light = normalize(LightSource[i].position.xyz - position.xyz);

        halfway =   normalize(light - view);
        product =   max(dot(fnormal, halfway), 0.0);
        fspecular = pow(product, frontMaterial.shininess);
		diffuse =   dot(light, fnormal);

		out_Color += ex_Color * frontMaterial.ambient
                    + ex_Color * frontMaterial.diffuse * abs(diffuse)
                    + tmpsp * fspecular;
//		if (diffuse > 0.0) {
//			out_Color += ex_Color * frontMaterial.diffuse * diffuse
//                        + tmpsp * fspecular;
//		}
	}
}

