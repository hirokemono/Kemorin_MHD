/*
//  c_ctl_data_PVR_colormap.c
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#include "c_ctl_data_PVR_colormap.h"

extern void * c_PVR_cmap_cbar_ctl_block_name(void *f_cmap_cbar_c);
extern void * c_PVR_cmap_cbar_ctl_iflag(void *f_cmap_cbar_c);
extern void * c_PVR_cmap_cbar_color_ctl(void *f_cmap_cbar_c);
extern void * c_PVR_cmap_cbar_cbar_ctl(void *f_cmap_cbar_c);

extern void * c_PVR_colormap_ctl_block_name(void *f_color);
extern void * c_PVR_colormap_ctl_iflag(void *f_color);
extern void * c_PVR_cmap_lic_color_field_ctl(void *f_color);
extern void * c_PVR_cmap_lic_color_comp_ctl(void *f_color);
extern void * c_PVR_cmap_lic_opacity_fld_ctl(void *f_color);
extern void * c_PVR_cmap_lic_opacity_comp_ctl(void *f_color);
extern void * c_PVR_cmap_colormap_mode_ctl(void *f_color);
extern void * c_PVR_cmap_data_mapping_ctl(void *f_color);
extern void * c_PVR_cmap_opacity_style_ctl(void *f_color);
extern void * c_PVR_cmap_range_min_ctl(void *f_color);
extern void * c_PVR_cmap_range_max_ctl(void *f_color);
extern void * c_PVR_cmap_fix_opacity_ctl(void *f_color);
extern void * c_PVR_cmap_colortbl_ctl(void *f_color);
extern void * c_PVR_cmap_linear_opacity_ctl(void *f_color);
extern void * c_PVR_cmap_background_color_ctl(void *f_color);

extern void * c_PVR_colorbar_ctl_block_name(void *f_cbar_ctl);
extern void * c_PVR_colorbar_ctl_iflag(void *f_cbar_ctl);
extern void * c_PVR_colorbar_switch_ctl(void *f_cbar_ctl);
extern void * c_PVR_colorbar_scale_ctl(void *f_cbar_ctl);
extern void * c_PVR_colorbar_position_ctl(void *f_cbar_ctl);
extern void * c_PVR_colorbar_zeromarker_ctl(void *f_cbar_ctl);
extern void * c_PVR_colorbar_font_size_ctl(void *f_cbar_ctl);
extern void * c_PVR_colorbar_ngrid_cbar_ctl(void *f_cbar_ctl);
extern void * c_PVR_colorbar_cbar_range_ctl(void *f_cbar_ctl);
extern void * c_PVR_colorbar_axis_switch_ctl(void *f_cbar_ctl);
extern void * c_PVR_colorbar_time_ctl(void *f_cbar_ctl);
extern void * c_PVR_colorbar_mapgrid_ctl(void *f_cbar_ctl);

extern void * c_PVR_light_ctl_block_name(void *f_light);
extern void * c_PVR_light_ctl_iflag(void *f_light);
extern void * c_PVR_light_ambient_coef_ctl(void *f_light);
extern void * c_PVR_light_diffuse_coef_ctl(void *f_light);
extern void * c_PVR_light_specular_coef_ctl(void *f_light);
extern void * c_PVR_light_position_ctl(void *f_light);
extern void * c_PVR_light_sph_posi_ctl(void *f_light);



struct colormap_ctl_c * init_f_colormap_ctl_c(void *(*c_load_self)(void *f_parent), 
                                              void *f_parent){
	struct colormap_ctl_c *cmap_c 
			= (struct colormap_ctl_c *) malloc(sizeof(struct colormap_ctl_c));
	if(cmap_c == NULL){
		printf("malloc error for colormap_ctl_c\n");
		exit(0);
    };
	cmap_c->f_self =  c_load_self(f_parent);
	cmap_c->f_iflag =   (int *) c_PVR_colormap_ctl_iflag(cmap_c->f_self);
	char *f_block_name = (char *) c_PVR_colormap_ctl_block_name(cmap_c->f_self);
    cmap_c->c_block_name = strngcopy_from_f(f_block_name);
    
    cmap_c->f_lic_color_fld_ctl =    init_f_ctl_chara_item(c_PVR_cmap_lic_color_field_ctl, cmap_c->f_self);
    cmap_c->f_lic_color_comp_ctl =   init_f_ctl_chara_item(c_PVR_cmap_lic_color_comp_ctl, cmap_c->f_self);
    cmap_c->f_lic_opacity_fld_ctl =  init_f_ctl_chara_item(c_PVR_cmap_lic_opacity_fld_ctl, cmap_c->f_self);
    cmap_c->f_lic_opacity_comp_ctl = init_f_ctl_chara_item(c_PVR_cmap_lic_opacity_comp_ctl, cmap_c->f_self);
    
    cmap_c->f_colormap_mode_ctl =    init_f_ctl_chara_item(c_PVR_cmap_colormap_mode_ctl, cmap_c->f_self);
    cmap_c->f_background_color_ctl = init_f_ctl_r3_item(c_PVR_cmap_background_color_ctl, cmap_c->f_self);
    cmap_c->f_data_mapping_ctl =     init_f_ctl_chara_item(c_PVR_cmap_data_mapping_ctl, cmap_c->f_self);
    cmap_c->f_colortbl_ctl =         init_f_ctl_r2_array(c_PVR_cmap_colortbl_ctl, cmap_c->f_self);
    cmap_c->f_opacity_style_ctl =    init_f_ctl_chara_item(c_PVR_cmap_opacity_style_ctl, cmap_c->f_self);
    cmap_c->f_fix_opacity_ctl =      init_f_ctl_real_item(c_PVR_cmap_fix_opacity_ctl, cmap_c->f_self);
    cmap_c->f_linear_opacity_ctl =   init_f_ctl_r2_array(c_PVR_cmap_linear_opacity_ctl, cmap_c->f_self);
    cmap_c->f_range_min_ctl =        init_f_ctl_real_item(c_PVR_cmap_range_min_ctl, cmap_c->f_self);
    cmap_c->f_range_max_ctl =        init_f_ctl_real_item(c_PVR_cmap_range_max_ctl, cmap_c->f_self);
    return cmap_c;
};

struct pvr_colorbar_ctl_c * init_f_pvr_colorbar_ctl_c(void *(*c_load_self)(void *f_parent), 
                                              void *f_parent){
	struct pvr_colorbar_ctl_c *cbar_c 
			= (struct pvr_colorbar_ctl_c *) malloc(sizeof(struct pvr_colorbar_ctl_c));
	if(cbar_c == NULL){
		printf("malloc error for pvr_colorbar_ctl_c\n");
		exit(0);
    };
	cbar_c->f_self =  c_load_self(f_parent);
	cbar_c->f_iflag =   (int *) c_PVR_colorbar_ctl_iflag(cbar_c->f_self);
	char *f_block_name = (char *) c_PVR_colorbar_ctl_block_name(cbar_c->f_self);
    cbar_c->c_block_name = strngcopy_from_f(f_block_name);
    
    cbar_c->f_colorbar_switch_ctl =   init_f_ctl_chara_item(c_PVR_colorbar_switch_ctl, cbar_c->f_self);
    cbar_c->f_colorbar_scale_ctl =    init_f_ctl_chara_item(c_PVR_colorbar_scale_ctl, cbar_c->f_self);
    cbar_c->f_colorbar_position_ctl = init_f_ctl_chara_item(c_PVR_colorbar_position_ctl, cbar_c->f_self);
    cbar_c->f_zeromarker_flag_ctl =   init_f_ctl_chara_item(c_PVR_colorbar_zeromarker_ctl, cbar_c->f_self);
    cbar_c->f_font_size_ctl =         init_f_ctl_int_item(c_PVR_colorbar_font_size_ctl, cbar_c->f_self);
    cbar_c->f_ngrid_cbar_ctl =        init_f_ctl_int_item(c_PVR_colorbar_ngrid_cbar_ctl, cbar_c->f_self);
    cbar_c->f_cbar_range_ctl =        init_f_ctl_r2_item(c_PVR_colorbar_cbar_range_ctl, cbar_c->f_self);
    
    cbar_c->f_axis_switch_ctl =    init_f_ctl_chara_item(c_PVR_colorbar_axis_switch_ctl, cbar_c->f_self);
    cbar_c->f_time_switch_ctl =    init_f_ctl_chara_item(c_PVR_colorbar_time_ctl, cbar_c->f_self);
    cbar_c->f_mapgrid_switch_ctl = init_f_ctl_chara_item(c_PVR_colorbar_mapgrid_ctl, cbar_c->f_self);
    return cbar_c;
};

struct pvr_colormap_bar_ctl_c * init_f_PVR_colormap_bar_ctl(char * ctl_file_name, 
                                                            void *(*c_load_self)(void *f_parent), 
                                                            void *f_parent)
{
	struct pvr_colormap_bar_ctl_c *f_cmap_cbar_c 
			= (struct pvr_colormap_bar_ctl_c *) malloc(sizeof(struct pvr_colormap_bar_ctl_c));
	if(f_cmap_cbar_c == NULL){
		printf("malloc error for pvr_colormap_bar_ctl_c\n");
		exit(0);
    };
    
	f_cmap_cbar_c->f_self =  c_load_self(f_parent);
	
	f_cmap_cbar_c->f_iflag =   (int *) c_PVR_cmap_cbar_ctl_iflag(f_cmap_cbar_c->f_self);
	char *f_block_name =   (char *) c_PVR_cmap_cbar_ctl_block_name(f_cmap_cbar_c->f_self);
	f_cmap_cbar_c->c_block_name = strngcopy_from_f(f_block_name);
    f_cmap_cbar_c->cmap_ctl_file_name =  ctl_file_name;
    
    f_cmap_cbar_c->cmap_c = init_f_colormap_ctl_c(c_PVR_cmap_cbar_color_ctl, f_cmap_cbar_c->f_self);
    f_cmap_cbar_c->cbar_c = init_f_pvr_colorbar_ctl_c(c_PVR_cmap_cbar_cbar_ctl, f_cmap_cbar_c->f_self);
    return f_cmap_cbar_c;
};

struct lighting_ctl_c * init_f_PVR_lighting_ctl(char * ctl_file_name, 
                                                void *(*c_load_self)(void *f_parent), 
                                                void *f_parent)
{
	struct lighting_ctl_c *f_light_c 
			= (struct lighting_ctl_c *) malloc(sizeof(struct lighting_ctl_c));
	if(f_light_c == NULL){
		printf("malloc error for lighting_ctl_c\n");
		exit(0);
    };
    
	f_light_c->f_self =  c_load_self(f_parent);
	
	f_light_c->f_iflag =   (int *) c_PVR_light_ctl_iflag(f_light_c->f_self);
	char *f_block_name =   (char *) c_PVR_light_ctl_block_name(f_light_c->f_self);
	f_light_c->c_block_name = strngcopy_from_f(f_block_name);
    f_light_c->light_ctl_file_name =  ctl_file_name;
    
    f_light_c->f_ambient_coef_ctl =   init_f_ctl_real_item(c_PVR_light_ambient_coef_ctl, f_light_c->f_self);
    f_light_c->f_diffuse_coef_ctl =   init_f_ctl_real_item(c_PVR_light_diffuse_coef_ctl, f_light_c->f_self);
    f_light_c->f_specular_coef_ctl =  init_f_ctl_real_item(c_PVR_light_specular_coef_ctl, f_light_c->f_self);
    f_light_c->f_light_position_ctl = init_f_ctl_r3_array(c_PVR_light_position_ctl, f_light_c->f_self);
    f_light_c->f_light_sph_posi_ctl = init_f_ctl_r3_array(c_PVR_light_sph_posi_ctl, f_light_c->f_self);
    return f_light_c;
};
