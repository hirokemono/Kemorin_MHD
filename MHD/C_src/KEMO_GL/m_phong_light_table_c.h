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

void alloc_phong_light_list(struct phong_lights *lights, int num);
void dealloc_phong_light_list(struct phong_lights *lights);
void realloc_phong_light_list(struct phong_lights *lights, int num);


void delete_phong_light_list(struct phong_lights *lights, int i_delete);
void add_phong_light_list(struct phong_lights *lights, float *add_xyz, float *add_rtp);

void light_positionfrom_angle(float *light_rtp, float *light_xyz);

#endif

