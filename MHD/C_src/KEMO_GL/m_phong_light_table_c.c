/*
// m_phong_light_table_c.c
*/

#include <stdio.h>
#include <stdlib.h>
#include "m_phong_light_table_c.h"

#define PI 3.131592

static void light_position_from_angle(struct view_element *view_s, float light_rtp[3], float light_xyz[4]){
	light_xyz[0] = (float) view_s->shift[0]
				+ light_rtp[0] * cos(PI * light_rtp[1]/180.0) * sin(PI * light_rtp[2]/180.0);
	light_xyz[1] = (float) view_s->shift[1]
				+ light_rtp[0] * sin(PI * light_rtp[1]/180.0);
	light_xyz[2] = (float) view_s->shift[2]
				+ light_rtp[0] * cos(PI * light_rtp[1]/180.0) * cos(PI * light_rtp[2]/180.0);
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

void add_phong_light_list(struct view_element *view_s, struct phong_lights *lights, 
						  int i_add, float r, float t, float p){
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
	
	light_position_from_angle(view_s, &lights->light_rtp[3*i_add], &lights->light_xyz[4*i_add]);
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


void init_phong_light_list(struct view_element *view_s, struct phong_lights *lights){
    int i;
    
	alloc_phong_light_list(lights, 3);
	
    for(i=0;i<3;i++){
		lights->ambient[i] =  0.15;
		lights->diffuse[i] =  0.35;
		lights->specular[i] = 0.05;
    }
    lights->ambient[3] =   1.0;
    lights->diffuse[3] =   1.0;
    lights->specular[3] =  1.0;
	lights->shiness[0] =  20.0;
	
	lights->light_rtp[0] = 10.0;
	lights->light_rtp[1] = 20.0;
	lights->light_rtp[2] = 30.0;
	
	lights->light_rtp[3] = 10.0;
	lights->light_rtp[4] = 45.0;
	lights->light_rtp[5] = 90.0;
	
	lights->light_rtp[6] = 10.0;
	lights->light_rtp[7] =-75.0;
	lights->light_rtp[8] =-75.0;
	
	light_position_from_angle(view_s, &lights->light_rtp[0], &lights->light_xyz[0]);
	light_position_from_angle(view_s, &lights->light_rtp[3], &lights->light_xyz[4]);
	light_position_from_angle(view_s, &lights->light_rtp[6], &lights->light_xyz[8]);
};


void set_each_light_position(struct view_element *view_s, struct phong_lights *lights, 
			int i_point, float r, float t, float p){
	lights->light_rtp[3*i_point  ] = r;
	lights->light_rtp[3*i_point+1] = t;
	lights->light_rtp[3*i_point+2] = p;
	light_position_from_angle(view_s, &lights->light_rtp[3*i_point], &lights->light_xyz[4*i_point]);
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
void send_each_light_xyz(struct phong_lights *lights,
                         int i_point, float *x, float *y, float *z){
    *x = lights->light_xyz[4*i_point  ];
    *y = lights->light_xyz[4*i_point+1];
    *z = lights->light_xyz[4*i_point+2];
    return;
};


void set_matrial_parameter(int itype, float value, struct phong_lights *lights){
    int i;
	if(itype == AMBIENT_FLAG){
        for(i=0;i<3;i++){lights->ambient[i] = value;};
        lights->ambient[3] = 1.0;
	}else if(itype == DIFFUSE_FLAG){
        for(i=0;i<3;i++){lights->diffuse[i] = value;};
        lights->diffuse[3] = 1.0;
	}else if(itype == SPECULAR_FLAG){
        for(i=0;i<3;i++){lights->specular[i] = value;};
        lights->specular[3] = 1.0;
	}else if(itype == SHINENESS_FLAG){
		lights->shiness[0] = value;
	};
	return;
};

float get_matrial_parameter(int itype, struct phong_lights *lights){
	float value = 0.0;
	if(itype == AMBIENT_FLAG){
		value = lights->ambient[0];
	}else if(itype == DIFFUSE_FLAG){
		value = lights->diffuse[0];
	}else if(itype == SPECULAR_FLAG){
		value = lights->specular[0];
	}else if(itype == SHINENESS_FLAG){
		value = lights->shiness[0];
	};
	return value;
};

