/*
// m_color_table_c.c
*/

#include <stdio.h>
#include <stdlib.h>
#include "m_color_table_c.h"

void alloc_color_index_list_s(struct colormap_params *cmap_s, int id_cmode){
	cmap_s->id_color_mode = id_cmode;
	cmap_s->colormap = init_real2_clist();
	cmap_s->opacitymap = init_real2_clist();
	cmap_s->max_opacity = ONE;
	cmap_s->min_opacity = ONE;
	
    cmap_s->single_color = (double *)calloc(4,sizeof(double));
    if (cmap_s->single_color == NULL){
        printf("malloc error for cmap_s->single_color\n");
        exit(1);
	};
	return;
}

void dealloc_color_index_list_s(struct colormap_params *cmap_s){
	dealloc_real2_clist(cmap_s->opacitymap);
	dealloc_real2_clist(cmap_s->colormap);
	free(cmap_s->single_color);
	free(cmap_s);
	return;
}

void delete_color_index_list_s(struct colormap_params *cmap_s, int i_delete){
	del_real2_clist_by_index(i_delete, cmap_s->colormap);
	return;
}

void delete_opacity_index_list_s(struct colormap_params *cmap_s, int i_delete){
	del_real2_clist_by_index(i_delete, cmap_s->opacitymap);
	return;
}

void add_color_index_list_s(struct colormap_params *cmap_s, double add_value, double add_color){
	add_real2_clist_between_value1(add_value, add_color, cmap_s->colormap);
	return;
}

void add_opacity_index_list_s(struct colormap_params *cmap_s, 
			double add_value, double add_opacity){
	add_real2_clist_between_value1(add_value, add_opacity, cmap_s->opacitymap);
	return;
}




static struct colormap_array * alloc_colormap_array(int num){
	struct colormap_array *cmap = (struct colormap_array *) malloc(sizeof(struct colormap_array));
    if (cmap  == NULL){
        printf("malloc error for colormap_array\n");
        exit(1);
	};
	
	cmap->num = num;
	cmap->data = (double *) calloc(cmap->num,sizeof(double));
    if (cmap->data == NULL){
        printf("malloc error for cmap->data\n");
        exit(1);
	};
	
	cmap->value = (double *)calloc(cmap->num,sizeof(double));
    if (cmap->value == NULL){
        printf("malloc error for cmap->value\n");
        exit(1);
	};
	
	return cmap;
}

struct colormap_array * init_colormap_from_list(struct real2_clist *colortbl_list){
	int i;
	struct colormap_array *cmap = alloc_colormap_array(count_real2_clist(colortbl_list));
	
	for(i=0;i<cmap->num;i++){
		set_from_real2_clist_at_index(i, colortbl_list, &cmap->data[i], &cmap->value[i]);
	};
	return cmap;
};

void dealloc_colormap_array(struct colormap_array *cmap){
	free(cmap->data);
	free(cmap->value);
	free(cmap);
	return;
};

