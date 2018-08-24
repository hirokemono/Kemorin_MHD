
/* m_color_table_c.c */

#include <stdio.h>
#include <stdlib.h>
#include "m_color_table_c.h"

void alloc_single_color_code(struct colormap_params *cmap_s){
    cmap_s->single_color = (double *)calloc(4,sizeof(double));
    return;
}

void alloc_color_index_list_s(struct colormap_params *cmap_s){
	cmap_s->colormap_mode = (struct chara_ctl_item *)malloc(sizeof(struct chara_ctl_item));
	alloc_chara_ctl_item_c(cmap_s->colormap_mode);
	
	cmap_s->colormap_clist = (struct real2_clist *)malloc(sizeof(struct real2_clist));
	init_real2_clist(cmap_s->colormap_clist);
    sprintf(cmap_s->colormap_clist->clist_name, "color map");
    sprintf(cmap_s->colormap_clist->r1_name, "data");
    sprintf(cmap_s->colormap_clist->r2_name, "color");
    
    cmap_s->range_min = (struct real_ctl_item *)malloc(sizeof(struct real_ctl_item));
    cmap_s->range_max = (struct real_ctl_item *)malloc(sizeof(struct real_ctl_item));
    init_real_ctl_item_c(cmap_s->range_min);
    init_real_ctl_item_c(cmap_s->range_max);
	return;
}

void alloc_opacity_index_list_s(struct colormap_params *cmap_s){
    cmap_s->opacitymap_clist = (struct real2_clist *)malloc(sizeof(struct real2_clist));
    init_real2_clist(cmap_s->opacitymap_clist);
    
	cmap_s->max_opacity = ONE;
	cmap_s->min_opacity = ZERO;
    sprintf(cmap_s->colormap_clist->clist_name, "opacity map");
    sprintf(cmap_s->opacitymap_clist->r1_name, "data");
    sprintf(cmap_s->opacitymap_clist->r2_name, "opacity");
    return;
}

void dealloc_single_color_code(struct colormap_params *cmap_s){
    free(cmap_s->single_color);
    return;
}

void dealloc_color_index_list_s(struct colormap_params *cmap_s){
    free(cmap_s->range_min);
    free(cmap_s->range_max);

    dealloc_chara_ctl_item_c(cmap_s->colormap_mode);
	free(cmap_s->colormap_mode);
	
	clear_real2_clist(cmap_s->colormap_clist);
	free(cmap_s->colormap_clist);
	return;
}

void dealloc_opacity_index_list_s(struct colormap_params *cmap_s){
    clear_real2_clist(cmap_s->opacitymap_clist);
    free(cmap_s->opacitymap_clist);
	return;
}


void delete_color_index_list_s(struct colormap_params *cmap_s, int i_delete){
	double r2_clst;
	
	int num = count_real2_clist(cmap_s->colormap_clist);
    if(num <= 2) return;
    if(i_delete >-1 && i_delete < num){
		del_real2_clist_by_index(i_delete, cmap_s->colormap_clist);
		
		if(i_delete == 0){
			set_from_real2_clist_at_index(i_delete, cmap_s->colormap_clist,
						&cmap_s->range_min->r_data, &r2_clst);
		} else if(i_delete == num-1){
			set_from_real2_clist_at_index(i_delete-1, cmap_s->colormap_clist,
						&cmap_s->range_max->r_data, &r2_clst);
		};
	};
    return;
}

void delete_opacity_index_list_s(struct colormap_params *cmap_s, int i_delete){
    int num = count_real2_clist(cmap_s->colormap_clist);
    if(num <= 2) return;
    if(i_delete < 0 || i_delete >= num){
        return;
    } else {
        del_real2_clist_by_index(i_delete, cmap_s->opacitymap_clist);
    };

    return;
}

void add_color_index_list_s(struct colormap_params *cmap_s, double add_value, double add_color){
    add_real2_clist_between_value1(add_value, add_color, cmap_s->colormap_clist);
    if(add_value < cmap_s->range_min->r_data) update_real_ctl_item_c(add_value, cmap_s->range_min);
    if(add_value > cmap_s->range_max->r_data) update_real_ctl_item_c(add_value, cmap_s->range_max);
    return;
}

void add_opacity_index_list_s(struct colormap_params *cmap_s, double add_value, double add_opacity){
    add_real2_clist_between_value1(add_value, add_opacity, cmap_s->opacitymap_clist);
	return;
}

