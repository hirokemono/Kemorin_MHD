
/* m_color_table_c.c */

#include <stdio.h>
#include <stdlib.h>
#include "m_color_table_c.h"

void alloc_single_color_code(struct colormap_params *cmap_s){
    cmap_s->single_color = (double *)calloc(4,sizeof(double));
    return;
}

void alloc_color_index_list_s(struct colormap_params *cmap_s, int id_cmode, int num){
	cmap_s->n_color_point =    num;
	cmap_s->nbuf_color_point = num;
	cmap_s->color_data = (double *)calloc(cmap_s->nbuf_color_point,sizeof(double));
	cmap_s->color_value = (double *)calloc(cmap_s->nbuf_color_point,sizeof(double));
	cmap_s->id_color_mode = id_cmode;
	return;
}

void alloc_opacity_index_list_s(struct colormap_params *cmap_s, int num){
	cmap_s->n_opacity_point =    num;
	cmap_s->nbuf_opacity_point = num;
	cmap_s->opacity_data =  (double *)calloc(cmap_s->nbuf_opacity_point,sizeof(double));
	cmap_s->opacity_value = (double *)calloc(cmap_s->nbuf_opacity_point,sizeof(double));
	cmap_s->max_opacity = ONE;
	cmap_s->min_opacity = ZERO;
	return;
}

void dealloc_single_color_code(struct colormap_params *cmap_s){
    free(cmap_s->single_color);
    return;
}

void dealloc_color_index_list_s(struct colormap_params *cmap_s){
	free(cmap_s->color_data);
	free(cmap_s->color_value);
	return;
}

void dealloc_opacity_index_list_s(struct colormap_params *cmap_s){
	free(cmap_s->opacity_data);
	free(cmap_s->opacity_value);
	return;
}


void realloc_color_index_list_s(struct colormap_params *cmap_s, int id_cmode, int num){
	if(num > cmap_s->nbuf_color_point){
		dealloc_color_index_list_s(cmap_s);
		alloc_color_index_list_s(cmap_s, id_cmode, num);
	} else {
		cmap_s->n_color_point = num;
	}
	cmap_s->id_color_mode = id_cmode;
	return;
}

void realloc_opacity_index_list_s(struct colormap_params *cmap_s, int num){
	if(num > cmap_s->nbuf_opacity_point){
		dealloc_opacity_index_list_s(cmap_s);
		alloc_opacity_index_list_s(cmap_s, num);
	} else {
		cmap_s->n_opacity_point = num;
	}
	cmap_s->max_opacity = ONE;
	cmap_s->min_opacity = ZERO;
	return;
}

void delete_color_index_list_s(struct colormap_params *cmap_s, int i_delete){
	double *color_data_tmp;
	double *color_value_tmp;
	int num, i;
	int id_cmode;
	
	if(cmap_s->nbuf_color_point <= 2) return;
	
	color_data_tmp = (double *)calloc(cmap_s->nbuf_color_point,sizeof(double));
	color_value_tmp = (double *)calloc(cmap_s->nbuf_color_point,sizeof(double));
	for(i = 0; i < cmap_s->nbuf_color_point; i++) {
		color_data_tmp[i] =  cmap_s->color_data[i];
		color_value_tmp[i] = cmap_s->color_value[i];
	}
	dealloc_color_index_list_s(cmap_s);
	
	num = cmap_s->nbuf_color_point - 1;
	id_cmode = cmap_s->id_color_mode;
	alloc_color_index_list_s(cmap_s, id_cmode, num);
	
	for(i = 0; i < i_delete-1; i++) {
		cmap_s->color_data[i] = color_data_tmp[i];
		cmap_s->color_value[i] = color_value_tmp[i];
	}
	
	for(i = i_delete; i < num; i++) {
		cmap_s->color_data[i] = color_data_tmp[i+1];
		cmap_s->color_value[i] = color_value_tmp[i+1];
	}
	
	free(color_data_tmp);
	free(color_value_tmp);
	return;
}

void delete_opacity_index_list_s(struct colormap_params *cmap_s, int i_delete){
	double *opacity_data_tmp;
	double *opacity_value_tmp;
	int num, i;
	
	if(cmap_s->nbuf_opacity_point <= 2) return;
	
	opacity_data_tmp = (double *)calloc(cmap_s->nbuf_opacity_point,sizeof(double));
	opacity_value_tmp = (double *)calloc(cmap_s->nbuf_opacity_point,sizeof(double));
	for(i = 0; i < cmap_s->nbuf_opacity_point; i++) {
		opacity_data_tmp[i] =  cmap_s->opacity_data[i];
		opacity_value_tmp[i] = cmap_s->opacity_value[i];
	}
	dealloc_opacity_index_list_s(cmap_s);
	
	num = cmap_s->nbuf_opacity_point - 1;
	alloc_opacity_index_list_s(cmap_s, num);
	
	for(i = 0; i < i_delete-1; i++) {
		cmap_s->opacity_data[i] = opacity_data_tmp[i];
		cmap_s->opacity_value[i] = opacity_value_tmp[i];
	}
	
	for(i = i_delete; i < num; i++) {
		cmap_s->opacity_data[i] = opacity_data_tmp[i+1];
		cmap_s->opacity_value[i] = opacity_value_tmp[i+1];
	}
	
	free(opacity_data_tmp);
	free(opacity_value_tmp);
	return;
}

void add_color_index_list_s(struct colormap_params *cmap_s, double add_value, double add_color){
	double *color_data_tmp;
	double *color_value_tmp;
	int num, i, i_add;
	int id_cmode;
	
	i_add = 0;
	for(i = 0; i < cmap_s->nbuf_color_point; i++) {
		if(add_value > cmap_s->color_data[i]) i_add = i+1;
	}
	
	color_data_tmp = (double *)calloc(cmap_s->nbuf_color_point,sizeof(double));
	color_value_tmp = (double *)calloc(cmap_s->nbuf_color_point,sizeof(double));
	for(i = 0; i < cmap_s->nbuf_color_point; i++) {
		color_data_tmp[i] =  cmap_s->color_data[i];
		color_value_tmp[i] = cmap_s->color_value[i];
	}
	dealloc_color_index_list_s(cmap_s);
	
	num = cmap_s->nbuf_color_point + 1;
	id_cmode = cmap_s->id_color_mode;
	alloc_color_index_list_s(cmap_s, id_cmode, num);
	
	for(i = 0; i < i_add; i++) {
		cmap_s->color_data[i] = color_data_tmp[i];
		cmap_s->color_value[i] = color_value_tmp[i];
	}
	
	cmap_s->color_data[i_add] = add_value;
	cmap_s->color_value[i_add] = add_color;
	
	for(i = i_add+1; i < num; i++) {
		cmap_s->color_data[i] = color_data_tmp[i-1];
		cmap_s->color_value[i] = color_value_tmp[i-1];
	}
	
	free(color_data_tmp);
	free(color_value_tmp);
	return;
}

void add_opacity_index_list_s(struct colormap_params *cmap_s, double add_value, double add_opacity){
	double *opacity_data_tmp;
	double *opacity_value_tmp;
	int num, i, i_add;
	
	i_add = 0;
	for(i = 0; i < cmap_s->nbuf_opacity_point; i++) {
		if(add_value > cmap_s->opacity_data[i]) i_add = i+1;
	}
	
	opacity_data_tmp = (double *)calloc(cmap_s->nbuf_opacity_point,sizeof(double));
	opacity_value_tmp = (double *)calloc(cmap_s->nbuf_opacity_point,sizeof(double));
	for(i = 0; i < cmap_s->nbuf_opacity_point; i++) {
		opacity_data_tmp[i] =  cmap_s->opacity_data[i];
		opacity_value_tmp[i] = cmap_s->opacity_value[i];
	}
	dealloc_opacity_index_list_s(cmap_s);
	
	num = cmap_s->nbuf_opacity_point + 1;
	alloc_opacity_index_list_s(cmap_s, num);
	
	for(i = 0; i < i_add; i++) {
		cmap_s->opacity_data[i] = opacity_data_tmp[i];
		cmap_s->opacity_value[i] = opacity_value_tmp[i];
	}
	
	cmap_s->opacity_data[i_add] = add_value;
	cmap_s->opacity_value[i_add] = add_opacity;
	
	for(i = i_add+1; i < num; i++) {
		cmap_s->opacity_data[i] = opacity_data_tmp[i-1];
		cmap_s->opacity_value[i] = opacity_value_tmp[i-1];
	}
	
	free(opacity_data_tmp);
	free(opacity_value_tmp);
	return;
}
