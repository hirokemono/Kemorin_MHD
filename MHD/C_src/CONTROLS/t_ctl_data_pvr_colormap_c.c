/*
//  t_ctl_data_pvr_colormap_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include "t_ctl_data_pvr_colormap_c.h"

struct colormap_ctl_c * init_colormap_ctl_c(){
    struct colormap_ctl_c *cmap_c;
    if((cmap_c = (struct colormap_ctl_c *) malloc(sizeof(struct colormap_ctl_c))) == NULL) {
        printf("malloc error for colormap_ctl_c \n");
        exit(0);
    }
    if((cmap_c->f_iflag = (int *)calloc(1, sizeof(int))) == NULL) {
        printf("malloc error for cmap_c->f_iflag\n");
        exit(0);
    }
    cmap_c->c_block_name = (char *)calloc(KCHARA_C, sizeof(char));
    
	cmap_c->f_colormap_mode_ctl =    init_chara_ctl_item_c();
    cmap_c->f_background_color_ctl = init_real3_ctl_item_c();
	
	cmap_c->f_lic_color_fld_ctl =    init_chara_ctl_item_c();
	cmap_c->f_lic_color_comp_ctl =   init_chara_ctl_item_c();
	cmap_c->f_lic_opacity_fld_ctl =  init_chara_ctl_item_c();
	cmap_c->f_lic_opacity_comp_ctl = init_chara_ctl_item_c();
	
	cmap_c->f_data_mapping_ctl = init_chara_ctl_item_c();
	
	cmap_c->f_colortbl_ctl = init_real2_clist();
    sprintf(cmap_c->f_colortbl_ctl->r1_name, "data");
    sprintf(cmap_c->f_colortbl_ctl->r2_name, "color");
	
	cmap_c->f_opacity_style_ctl = init_chara_ctl_item_c();
	
    cmap_c->f_fix_opacity_ctl = init_real_ctl_item_c();
	
	cmap_c->f_linear_opacity_ctl = init_real2_clist();
    sprintf(cmap_c->f_linear_opacity_ctl->r1_name, "data");
    sprintf(cmap_c->f_linear_opacity_ctl->r2_name, "opacity");
	
    cmap_c->f_range_min_ctl = init_real_ctl_item_c();
    cmap_c->f_range_max_ctl = init_real_ctl_item_c();
	
	return cmap_c;
};

void dealloc_colormap_ctl_c(struct colormap_ctl_c *cmap_c){
	
	dealloc_chara_ctl_item_c(cmap_c->f_colormap_mode_ctl);
    dealloc_real3_ctl_item(cmap_c->f_background_color_ctl);
	
	dealloc_chara_ctl_item_c(cmap_c->f_lic_color_fld_ctl);
	dealloc_chara_ctl_item_c(cmap_c->f_lic_color_comp_ctl);
	dealloc_chara_ctl_item_c(cmap_c->f_lic_opacity_fld_ctl);
	dealloc_chara_ctl_item_c(cmap_c->f_lic_opacity_comp_ctl);
	
	dealloc_chara_ctl_item_c(cmap_c->f_data_mapping_ctl);
	
	dealloc_real2_clist(cmap_c->f_colortbl_ctl);
	
	dealloc_chara_ctl_item_c(cmap_c->f_opacity_style_ctl);
    dealloc_real_ctl_item_c(cmap_c->f_fix_opacity_ctl);
	
	dealloc_real2_clist(cmap_c->f_linear_opacity_ctl);
	
    dealloc_real_ctl_item_c(cmap_c->f_range_min_ctl);
    dealloc_real_ctl_item_c(cmap_c->f_range_max_ctl);
	
    free(cmap_c->c_block_name);
    free(cmap_c->f_iflag);
    free(cmap_c);
	return;
};

struct lighting_ctl_c * init_lighting_ctl_c(void){
    struct lighting_ctl_c *light_c;
    if((light_c = (struct lighting_ctl_c *) malloc(sizeof(struct lighting_ctl_c))) == NULL) {
        printf("malloc error for lighting_ctl_c \n");
        exit(0);
    }
    if((light_c->f_iflag = (int *)calloc(1, sizeof(int))) == NULL) {
        printf("malloc error for light_c->f_iflag\n");
        exit(0);
    }
    light_c->c_block_name = (char *)calloc(KCHARA_C, sizeof(char));
	
    light_c->f_ambient_coef_ctl =  init_real_ctl_item_c();
    light_c->f_diffuse_coef_ctl =  init_real_ctl_item_c();
    light_c->f_specular_coef_ctl = init_real_ctl_item_c();
	
    light_c->f_light_position_ctl = init_real3_clist();
    sprintf(light_c->f_light_position_ctl->r1_name, "x");
    sprintf(light_c->f_light_position_ctl->r2_name, "y");
    sprintf(light_c->f_light_position_ctl->r3_name, "z");
	
	return light_c;
};

void dealloc_lighting_ctl_c(struct lighting_ctl_c *light_c){
	free(light_c->f_ambient_coef_ctl);
	free(light_c->f_diffuse_coef_ctl);
	free(light_c->f_specular_coef_ctl);
	
	dealloc_real3_clist(light_c->f_light_position_ctl);

    free(light_c->c_block_name);
    free(light_c->f_iflag);
    free(light_c);
    return;
};


struct pvr_colorbar_ctl_c * init_colorbar_ctl_c(void){
    struct pvr_colorbar_ctl_c *cbar_c;
    if((cbar_c = (struct pvr_colorbar_ctl_c *) malloc(sizeof(struct pvr_colorbar_ctl_c))) == NULL) {
        printf("malloc error for pvr_colorbar_ctl_c \n");
        exit(0);
    }
    if((cbar_c->f_iflag = (int *)calloc(1, sizeof(int))) == NULL) {
        printf("malloc error for cbar_c->f_iflag\n");
        exit(0);
    }
    cbar_c->c_block_name = (char *)calloc(KCHARA_C, sizeof(char));
	
	cbar_c->f_colorbar_switch_ctl =    init_chara_ctl_item_c();
	cbar_c->f_colorbar_scale_ctl =     init_chara_ctl_item_c();
    cbar_c->f_colorbar_position_ctl =  init_chara_ctl_item_c();
	cbar_c->f_zeromarker_flag_ctl =    init_chara_ctl_item_c();
	
    cbar_c->f_font_size_ctl =  init_int_ctl_item_c();
    cbar_c->f_ngrid_cbar_ctl = init_int_ctl_item_c();
	
	cbar_c->f_cbar_range_ctl = init_real2_ctl_item_c();
	
	cbar_c->f_axis_switch_ctl = init_chara_ctl_item_c();
    cbar_c->f_time_switch_ctl = init_chara_ctl_item_c();
    cbar_c->f_mapgrid_switch_ctl = init_chara_ctl_item_c();
	
	return cbar_c;
};

void dealloc_colorbar_ctl_c(struct pvr_colorbar_ctl_c *cbar_c){
	
	dealloc_chara_ctl_item_c(cbar_c->f_colorbar_switch_ctl);
	dealloc_chara_ctl_item_c(cbar_c->f_colorbar_scale_ctl);
    dealloc_chara_ctl_item_c(cbar_c->f_colorbar_position_ctl);
	dealloc_chara_ctl_item_c(cbar_c->f_zeromarker_flag_ctl);
	
	free(cbar_c->f_font_size_ctl);
	free(cbar_c->f_ngrid_cbar_ctl);
	
	free(cbar_c->f_cbar_range_ctl);
	
	dealloc_chara_ctl_item_c(cbar_c->f_axis_switch_ctl);
    dealloc_chara_ctl_item_c(cbar_c->f_time_switch_ctl);
    dealloc_chara_ctl_item_c(cbar_c->f_mapgrid_switch_ctl);
    free(cbar_c->c_block_name);
    free(cbar_c->f_iflag);
    free(cbar_c);
	return;
};


struct pvr_colormap_bar_ctl_c * init_colormap_colorbar_ctl_c(void){
    struct pvr_colormap_bar_ctl_c *cmap_cbar_c;
    if((cmap_cbar_c = (struct pvr_colormap_bar_ctl_c *) malloc(sizeof(struct pvr_colormap_bar_ctl_c))) == NULL) {
        printf("malloc error for pvr_colormap_bar_ctl_c \n");
        exit(0);
    }
    if((cmap_cbar_c->f_iflag = (int *)calloc(1, sizeof(int))) == NULL) {
        printf("malloc error for cmap_cbar_c->f_iflag\n");
        exit(0);
    }
    cmap_cbar_c->c_block_name = (char *)calloc(KCHARA_C, sizeof(char));
    
	cmap_cbar_c->cmap_c = init_colormap_ctl_c();
	cmap_cbar_c->cbar_c = init_colorbar_ctl_c();	
	return cmap_cbar_c;
};

void dealloc_colormap_colorbar_ctl_c(struct pvr_colormap_bar_ctl_c *cmap_cbar_c){
	dealloc_colormap_ctl_c(cmap_cbar_c->cmap_c);
	dealloc_colorbar_ctl_c(cmap_cbar_c->cbar_c);
	
    free(cmap_cbar_c->c_block_name);
    free(cmap_cbar_c->f_iflag);
    free(cmap_cbar_c);
	return;
};
