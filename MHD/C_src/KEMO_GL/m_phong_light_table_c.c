/*
// m_phong_light_table_c.c
*/

#include <stdio.h>
#include <stdlib.h>
#include "m_phong_light_table_c.h"

#define PI 3.131592

void light_positionfrom_angle(float light_rtp[3], float light_xyz[4]){
	light_xyz[0] = light_rtp[0] * cos(PI * light_rtp[1]/180.0) * sin(PI * light_rtp[2]/180.0);
	light_xyz[1] = light_rtp[0] * sin(PI * light_rtp[1]/180.0);
	light_xyz[2] = light_rtp[0] * cos(PI * light_rtp[1]/180.0) * cos(PI * light_rtp[2]/180.0);
	light_xyz[3] = 1.0;
	return;
};

void alloc_phong_light_list(struct phong_lights *lights, int num){
	lights->n_light_point =    num;
	lights->nbuf_light_point = num;
	lights->light_xyz = (float *)calloc(4*lights->nbuf_light_point,sizeof(float));
	lights->light_rtp = (float *)calloc(3*lights->nbuf_light_point,sizeof(float));
	return;
}

void dealloc_phong_light_list(struct phong_lights *lights){
	free(lights->light_xyz);
	free(lights->light_rtp);
	return;
}

void realloc_phong_light_list(struct phong_lights *lights, int num){
	if(num > lights->nbuf_light_point){
		dealloc_phong_light_list(lights);
		alloc_phong_light_list(lights, num);
	} else {
		lights->n_light_point = num;
	}
	return;
}

void delete_phong_light_list(struct phong_lights *lights, int i_delete){
	float *light_xyz_tmp;
	float *light_rtp_tmp;
	int i;
	
	if(lights->nbuf_light_point <= 1) return;
	
	light_xyz_tmp = (float *)calloc(4*lights->nbuf_light_point,sizeof(float));
	light_rtp_tmp = (float *)calloc(3*lights->nbuf_light_point,sizeof(float));
	for(i=0;i< 4*lights->nbuf_light_point;i++){
		light_xyz_tmp[i] =  lights->light_xyz[i];
	};
	for(i=0;i< 3*lights->nbuf_light_point;i++){
		light_rtp_tmp[i] = lights->light_rtp[i];
	}
	dealloc_phong_light_list(lights);
	
	alloc_phong_light_list(lights, (lights->nbuf_light_point-1));
	
	for(i=0;i< 4*i_delete;i++) {
		lights->light_xyz[i] = light_xyz_tmp[i];
	};
	for(i=0;i< 3*i_delete;i++) {
		lights->light_rtp[i] = light_rtp_tmp[i];
	}
	
	for(i=4*i_delete;i< 4*lights->nbuf_light_point;i++) {
		lights->light_xyz[i] = light_xyz_tmp[i+4];
	};
	for(i=3*i_delete;i< 3*lights->nbuf_light_point;i++) {
		lights->light_rtp[i] = light_rtp_tmp[i+3];
	}
	
	free(light_xyz_tmp);
	free(light_rtp_tmp);
	return;
}

void add_phong_light_list(struct phong_lights *lights, int i_add,
						  float r, float t, float p){
	float *light_xyz_tmp;
	float *light_rtp_tmp;
	int i;
	
	light_xyz_tmp = (float *)calloc(4*lights->nbuf_light_point,sizeof(float));
	light_rtp_tmp = (float *)calloc(3*lights->nbuf_light_point,sizeof(float));
	for(i=0;i< 4*lights->nbuf_light_point;i++){
		light_xyz_tmp[i] =  lights->light_xyz[i];
	};
	for(i=0;i< 3*lights->nbuf_light_point;i++){
		light_rtp_tmp[i] = lights->light_rtp[i];
	}
	dealloc_phong_light_list(lights);
	
	alloc_phong_light_list(lights, (lights->nbuf_light_point+1));
	
	for(i=0;i< 4*i_add;i++) {lights->light_xyz[i] = light_xyz_tmp[i];};
	for(i=0;i< 3*i_add;i++) {lights->light_rtp[i] = light_rtp_tmp[i];};
	
	lights->light_rtp[3*i_add  ] = r;
	lights->light_rtp[3*i_add+1] = t;
	lights->light_rtp[3*i_add+2] = p;
	
	light_positionfrom_angle(&lights->light_rtp[3*i_add], &lights->light_xyz[4*i_add]);
	for(i=4*(i_add+1);i<4*lights->nbuf_light_point; i++){
		lights->light_xyz[i] = light_xyz_tmp[i-4];
	};
	for(i=3*(i_add+1);i<3*lights->nbuf_light_point; i++){
		lights->light_rtp[i] = light_rtp_tmp[i-3];
	};
	
	free(light_xyz_tmp);
	free(light_rtp_tmp);
	return;
}


void init_phong_light_list(struct phong_lights *lights){
	alloc_phong_light_list(lights, 1);
	
	lights->ambient =  0.3;
	lights->diffuse =  0.8;
	lights->specular = 0.6;
	lights->shiness = 6.0;
	
	lights->light_rtp[0] = 20.0;
	lights->light_rtp[1] = 30.0;
	lights->light_rtp[2] = 90.0;
	light_positionfrom_angle(&lights->light_rtp[0], &lights->light_xyz[0]);
};


void set_each_light_position(struct phong_lights *lights, 
			int i_point, float r, float t, float p){
	lights->light_rtp[3*i_point  ] = r;
	lights->light_rtp[3*i_point+1] = t;
	lights->light_rtp[3*i_point+2] = p;
	light_positionfrom_angle(&lights->light_rtp[3*i_point], &lights->light_xyz[4*i_point]);
	return;
}

int send_num_light_position(struct phong_lights *lights){return lights->n_light_point;};
void send_each_light_rtp(struct phong_lights *lights, 
			int i_point, float *r, float *t, float *p){
	*r = lights->light_rtp[3*i_point  ];
	*t = lights->light_rtp[3*i_point+1];
	*p = lights->light_rtp[3*i_point+2];
	return;
}

void set_material_ambient(struct phong_lights *lights, float ambient_in){
	lights->ambient = ambient_in;
	return;
};
void set_material_diffuse(struct phong_lights *lights, float diffuse_in){
	lights->diffuse = diffuse_in;
	return;
};
void set_material_specular(struct phong_lights *lights, float specular_in){
	lights->specular = specular_in;
	return;
};
void set_material_shineness(struct phong_lights *lights, float shiness_in){
	lights->shiness = shiness_in;
	return;
};

float send_material_ambient(struct phong_lights *lights){return lights->ambient;};
float send_material_diffuse(struct phong_lights *lights){return lights->diffuse;};
float send_material_specular(struct phong_lights *lights){return lights->specular;};
float send_material_shiness(struct phong_lights *lights){return lights->shiness;};
