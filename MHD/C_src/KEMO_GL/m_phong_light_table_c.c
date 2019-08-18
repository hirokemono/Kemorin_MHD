/*
// m_phong_light_table_c.c
*/

#include <stdio.h>
#include <stdlib.h>
#include "m_phong_light_table_c.h"

#define PI 3.131592

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
	int num, nd, i;
	
	if(lights->nbuf_light_point <= 2) return;
	
	light_xyz_tmp = (float *)calloc(3*lights->nbuf_light_point,sizeof(float));
	light_rtp_tmp = (float *)calloc(3*lights->nbuf_light_point,sizeof(float));
	for(i = 0; i < lights->nbuf_light_point; i++) {
		for(nd=0;nd<3;nd++){
			light_xyz_tmp[3*i+nd] =  lights->light_xyz[4*i+nd];
			light_rtp_tmp[3*i+nd] = lights->light_rtp[3*i+nd];
		};
	}
	dealloc_phong_light_list(lights);
	
	num = lights->nbuf_light_point - 1;
	alloc_phong_light_list(lights, num);
	
	for(i = 0; i < i_delete; i++) {
		for(nd=0;nd<3;nd++){
			lights->light_xyz[4*i+nd] = light_xyz_tmp[3*i+nd];
			lights->light_rtp[3*i+nd] = light_rtp_tmp[3*i+nd];
		};
	}
	
	for(i = i_delete+1; i < num+1; i++) {
		for(nd=0;nd<3;nd++){
			lights->light_xyz[4*(i-1)+nd] = light_xyz_tmp[3*i+nd];
			lights->light_rtp[3*(i-1)+nd] = light_rtp_tmp[3*i+nd];
		};
	}
	
	for(i = 0; i < num+1; i++) {lights->light_xyz[4*i+3] = 1.0;};
	free(light_xyz_tmp);
	free(light_rtp_tmp);
	return;
}

void add_phong_light_list(struct phong_lights *lights, float *add_xyz, float *add_rtp){
	float *light_xyz_tmp;
	float *light_rtp_tmp;
	int num, nd, i, i_add;
	
	i_add = 0;
	if(lights->nbuf_light_point == 10) return;
	for(i = 0; i < lights->nbuf_light_point; i++) {
		for(nd=0;nd<3;nd++){
			if(add_rtp[0] > lights->light_rtp[3*i]) {i_add = i+1;};
		};
	}
	
	light_xyz_tmp = (float *)calloc(3*lights->nbuf_light_point,sizeof(float));
	light_rtp_tmp = (float *)calloc(3*lights->nbuf_light_point,sizeof(float));
	for(i = 0; i < lights->nbuf_light_point; i++) {
		for(nd=0;nd<3;nd++){
			light_xyz_tmp[3*i+nd] = lights->light_xyz[4*i+nd];
			light_rtp_tmp[3*i+nd] = lights->light_rtp[3*i+nd];
		};
	};
	dealloc_phong_light_list(lights);
	
	num = lights->nbuf_light_point + 1;
	alloc_phong_light_list(lights, num);
	
	for(i = 0; i < i_add; i++) {
		for(nd=0;nd<3;nd++){
			lights->light_xyz[4*i+nd] = light_xyz_tmp[3*i+nd];
			lights->light_rtp[3*i+nd] = light_rtp_tmp[3*i+nd];
		};
	}
	
	for(nd=0;nd<3;nd++){
		lights->light_xyz[4*i_add+nd] = add_xyz[nd];
		lights->light_rtp[3*i_add+nd] = add_rtp[nd];
	};
	
	for(i = i_add+1; i < num; i++) {
		for(nd=0;nd<3;nd++){
			lights->light_xyz[4*i+nd] = light_xyz_tmp[3*(i-1)+nd];
			lights->light_rtp[3*i+nd] = light_rtp_tmp[3*(i-1)+nd];
		};
	}
	
	for(i = 0; i < num; i++) {lights->light_xyz[4*i+3] = 1.0;};
	free(light_xyz_tmp);
	free(light_rtp_tmp);
	return;
}

void light_positionfrom_angle(float *light_rtp, float *light_xyz){
	light_xyz[0] = light_rtp[0] * cos(PI * light_rtp[1]/180.0) * sin(PI * light_rtp[2]/180.0);
	light_xyz[1] = light_rtp[0] * sin(PI * light_rtp[1]/180.0);
	light_xyz[2] = light_rtp[0] * cos(PI * light_rtp[1]/180.0) * cos(PI * light_rtp[2]/180.0);
	light_xyz[3] = 1.0;
	return;
};


void init_phong_light_list(struct phong_lights *lights){
	alloc_phong_light_list(lights, 1);
	
	lights->ambient =  0.3;
	lights->diffuse =  0.8;
	lights->specular = 0.6;
	lights->shiness = 6.0;
	
	lights->light_rtp[0] =   20.0;
	lights->light_rtp[1] =   30.0;
	lights->light_rtp[2] =  90.0;
	light_positionfrom_angle(&lights->light_rtp[0], &lights->light_xyz[0]);
};
