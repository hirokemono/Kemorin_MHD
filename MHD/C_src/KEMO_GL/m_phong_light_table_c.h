/*
// m_phong_light_table_c.h
*/

#ifndef M_PHONG_LIGHT_TABLE_C_
#define M_PHONG_LIGHT_TABLE_C_

#include <math.h>

struct phong_lights{
	int n_light_point, nbuf_light_point;
	float *light_xyz;
	float *light_rtp;
	
	float ambient;
	float diffuse;
	float specular;
	float shiness;
};

/* prototypes */

void light_positionfrom_angle(float *light_rtp, float *light_xyz);


void alloc_phong_light_list(struct phong_lights *lights, int num);
void dealloc_phong_light_list(struct phong_lights *lights);
void realloc_phong_light_list(struct phong_lights *lights, int num);

void delete_phong_light_list(struct phong_lights *lights, int i_delete);
void add_phong_light_list(struct phong_lights *lights, int i_add,
						  float r, float t, float p);

void init_phong_light_list(struct phong_lights *lights);



void set_each_light_position(struct phong_lights *lights, 
			int i_point, float r, float t, float p);
int send_num_light_position(struct phong_lights *lights);
void send_each_light_rtp(struct phong_lights *lights, 
			int i_point, float *r, float *t, float *p);

void set_material_ambient(struct phong_lights *lights, float ambient_in);
void set_material_diffuse(struct phong_lights *lights, float diffuse_in);
void set_material_specular(struct phong_lights *lights, float specular_in);
void set_material_shineness(struct phong_lights *lights, float shiness_in);

float send_material_ambient(struct phong_lights *lights);
float send_material_diffuse(struct phong_lights *lights);
float send_material_specular(struct phong_lights *lights);
float send_material_shiness(struct phong_lights *lights);

#endif

