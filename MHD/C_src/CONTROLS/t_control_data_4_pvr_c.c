/*
//  t_control_data_4_pvr_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "t_control_data_4_pvr_c.h"

struct pvr_plot_area_ctl_c * init_pvr_plot_area_ctl_c(){
    struct pvr_plot_area_ctl_c *area_c;
    if((area_c = (struct pvr_plot_area_ctl_c *) malloc(sizeof(struct pvr_plot_area_ctl_c))) == NULL) {
        printf("malloc error for pvr_plot_area_ctl_c \n");
        exit(0);
    }
	if((area_c->f_iflag = (int *)calloc(1, sizeof(int))) == NULL) {
		printf("malloc error for area_c->f_iflag\n");
		exit(0);
	}
    area_c->c_block_name = (char *)calloc(KCHARA_C, sizeof(char));
	
	area_c->label_pvr_area =   init_label_pvr_area();
    area_c->pvr_area_list =    init_chara_clist();
    area_c->surf_enhanse_ctl = init_chara2_real_clist();
    
    sprintf(area_c->surf_enhanse_ctl->c1_name, "Group_name");
    sprintf(area_c->surf_enhanse_ctl->c2_name, "Direction");
    sprintf(area_c->surf_enhanse_ctl->r1_name, "Opacity");
	
	return area_c;
};

void dealloc_pvr_plot_area_ctl_c(struct pvr_plot_area_ctl_c *area_c){
	
	dealloc_control_labels_f(area_c->label_pvr_area);
	dealloc_chara_clist(area_c->pvr_area_list);
	dealloc_chara2_real_clist(area_c->surf_enhanse_ctl);
    
    free(area_c->c_block_name);
    area_c->f_iflag = NULL;
	free(area_c);
	return;
};


struct pvr_ctl_c * init_pvr_ctl_c(){
    struct pvr_ctl_c *pvr_c;
    if((pvr_c = (struct pvr_ctl_c *) malloc(sizeof(struct pvr_ctl_c))) == NULL) {
        printf("malloc error for pvr_ctl_c \n");
        exit(0);
    }
	if((pvr_c->f_iflag = (int *)calloc(1, sizeof(int))) == NULL) {
		printf("malloc error for pvr_c->f_iflag\n");
		exit(0);
	}
    pvr_c->c_block_name = (char *)calloc(KCHARA_C, sizeof(char));
	
	pvr_c->label_pvr_ctl_w_dpl = init_label_pvr_ctl_w_dpl();
	pvr_c->updated_ctl = init_chara_ctl_item_c();
	
	pvr_c->file_head_ctl = init_chara_ctl_item_c();
	pvr_c->file_fmt_ctl = init_chara_ctl_item_c();
	pvr_c->monitoring_ctl = init_chara_ctl_item_c();
	
	pvr_c->streo_ctl = init_chara_ctl_item_c();
    pvr_c->anaglyph_ctl = init_chara_ctl_item_c();
	pvr_c->quilt_ctl = init_chara_ctl_item_c();
	
	pvr_c->pvr_field_ctl = init_chara_ctl_item_c();
	pvr_c->pvr_comp_ctl = init_chara_ctl_item_c();
	
	pvr_c->area_c = init_pvr_plot_area_ctl_c();
	
    pvr_c->pvr_modelview_file_name = (char *)calloc(KCHARA_C, sizeof(char));
	pvr_c->mat_c = init_modelview_ctl_c();
	
	pvr_c->light_c = init_lighting_ctl_c();
	
    pvr_c->pvr_colormap_file_name = (char *)calloc(KCHARA_C, sizeof(char));
	pvr_c->cmap_cbar_c = init_colormap_colorbar_ctl_c();
	
	pvr_c->movie_c = init_pvr_movie_ctl_c();
	
	init_pvr_section_ctl_list(&pvr_c->pvr_sect_c_list);
	init_pvr_iso_ctl_list(&pvr_c->pvr_iso_c_list);
	
	return pvr_c;
};

void dealloc_pvr_ctl_c(struct pvr_ctl_c *pvr_c){
    dealloc_control_labels_f(pvr_c->label_pvr_ctl_w_dpl);
	
	dealloc_chara_ctl_item_c(pvr_c->updated_ctl);
	
	dealloc_chara_ctl_item_c(pvr_c->file_head_ctl);
	dealloc_chara_ctl_item_c(pvr_c->file_fmt_ctl);
	dealloc_chara_ctl_item_c(pvr_c->monitoring_ctl);
	
	dealloc_chara_ctl_item_c(pvr_c->streo_ctl);
    dealloc_chara_ctl_item_c(pvr_c->anaglyph_ctl);
	dealloc_chara_ctl_item_c(pvr_c->quilt_ctl);
	
	dealloc_chara_ctl_item_c(pvr_c->pvr_field_ctl);
	dealloc_chara_ctl_item_c(pvr_c->pvr_comp_ctl);
	
    
	dealloc_pvr_plot_area_ctl_c(pvr_c->area_c);
	
	dealloc_modelview_ctl_c(pvr_c->mat_c);
	free(pvr_c->mat_c);
    free(pvr_c->pvr_modelview_file_name);
	
	dealloc_lighting_ctl_c(pvr_c->light_c);
	
	dealloc_colormap_colorbar_ctl_c(pvr_c->cmap_cbar_c);
    free(pvr_c->pvr_colormap_file_name);
	
	dealloc_pvr_movie_ctl_c(pvr_c->movie_c);
	
	clear_pvr_section_ctl_list(&pvr_c->pvr_sect_c_list);
	clear_pvr_iso_ctl_list(&pvr_c->pvr_iso_c_list);
		
    free(pvr_c->c_block_name);
    pvr_c->f_iflag = NULL;
    free(pvr_c);
	return;
};

