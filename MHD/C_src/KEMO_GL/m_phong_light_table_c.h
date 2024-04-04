/*
// m_phong_light_table_c.h
*/

#ifndef M_PHONG_LIGHT_TABLE_C_
#define M_PHONG_LIGHT_TABLE_C_

#include <math.h>
#include "kemoviewer.h"
#include "m_gl_transfer_matrix.h"

struct phong_lights{
	int n_light_point, nbuf_light_point;
	float *light_xyz;
	float *light_rtp;
	
	float ambient[4];
	float diffuse[4];
	float specular[4];
	float shiness[1];
};

/* prototypes */

void alloc_phong_light_list(struct phong_lights *lights, int num);
void dealloc_phong_light_list(struct phong_lights *lights);
void realloc_phong_light_list(struct phong_lights *lights, int num);

void delete_phong_light_list(struct phong_lights *lights, int i_delete);
void add_phong_light_list(struct view_element *view_s, struct phong_lights *lights, int i_add,
						  float r, float t, float p);
void init_phong_light_list(struct view_element *view_s, struct phong_lights *lights);
void set_each_light_position(struct view_element *view_s, struct phong_lights *lights, 
			int i_point, float r, float t, float p);

int send_num_light_position(struct phong_lights *lights);
void send_each_light_rtp(struct phong_lights *lights, 
                         int i_point, float *r, float *t, float *p);
void send_each_light_xyz(struct phong_lights *lights,
                         int i_point, float *x, float *y, float *z);

void set_matrial_parameter(int itype, float value, struct phong_lights *lights);
float get_matrial_parameter(int itype, struct phong_lights *lights);

#endif

