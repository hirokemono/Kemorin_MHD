
/* m_color_table_c.c */

#include <stdio.h>
#include <stdlib.h>
#include "m_color_table_c.h"

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

