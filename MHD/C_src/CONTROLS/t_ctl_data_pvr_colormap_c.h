/*
//  t_ctl_data_pvr_colormap_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#ifndef t_ctl_data_pvr_colormap_c_h_
#define t_ctl_data_pvr_colormap_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"
#include "t_control_real2_IO.h"
#include "t_control_real3_IO.h"

#define NLBL_COLORMAP_CTL     14
#define NLBL_LIGHTING_CTL      5
#define NLBL_PVR_COLORBAR_CTL 10
#define NLBL_CMAP_CBAR_CTL     2

struct colormap_ctl_c{
    void * f_self;
    int * f_iflag;
    
    char *c_block_name;
	
	struct chara_ctl_item *f_colormap_mode_ctl;
	
	struct chara_ctl_item *f_lic_color_fld_ctl;
	struct chara_ctl_item *f_lic_color_comp_ctl;
	struct chara_ctl_item *f_lic_opacity_fld_ctl;
	struct chara_ctl_item *f_lic_opacity_comp_ctl;
	
    struct real3_ctl_item *f_background_color_ctl;
    
	struct chara_ctl_item *f_data_mapping_ctl;
	struct real2_clist    *f_colortbl_ctl;

	struct chara_ctl_item *f_opacity_style_ctl;
	struct real_ctl_item  *f_fix_opacity_ctl;
	struct real2_clist    *f_linear_opacity_ctl;
	
	struct real_ctl_item *f_range_min_ctl;
	struct real_ctl_item *f_range_max_ctl;
};

struct pvr_colorbar_ctl_c{
    void * f_self;
    int * f_iflag;
    
    char *c_block_name;
	
	struct chara_ctl_item *f_colorbar_switch_ctl;
	struct chara_ctl_item *f_colorbar_scale_ctl;
    struct chara_ctl_item *f_colorbar_position_ctl;
	struct chara_ctl_item *f_zeromarker_flag_ctl;
	
	struct int_ctl_item *f_font_size_ctl;
	struct int_ctl_item *f_ngrid_cbar_ctl;
	
	struct real2_ctl_item *f_cbar_range_ctl;
	
	struct chara_ctl_item *f_axis_switch_ctl;
    struct chara_ctl_item *f_time_switch_ctl;
    struct chara_ctl_item *f_mapgrid_switch_ctl;
};

struct pvr_colormap_bar_ctl_c{
    void * f_self;
    int * f_iflag;
    
    char *c_block_name;
    char *cmap_ctl_file_name;
   
	struct colormap_ctl_c *cmap_c;
	struct pvr_colorbar_ctl_c *cbar_c;
};


struct lighting_ctl_c{
    void * f_self;
    int * f_iflag;
    
    char *c_block_name;
    char *light_ctl_file_name;
    
    struct real_ctl_item *f_ambient_coef_ctl;
    struct real_ctl_item *f_diffuse_coef_ctl;
    struct real_ctl_item *f_specular_coef_ctl;
    
    struct real3_clist *f_light_position_ctl;
    struct real3_clist *f_light_sph_posi_ctl;
};


/* prototypes */
struct colormap_ctl_c * init_colormap_ctl_c(void);
void dealloc_colormap_ctl_c(struct colormap_ctl_c *cmap_c);

struct lighting_ctl_c * init_lighting_ctl_c(void);
void dealloc_lighting_ctl_c(struct lighting_ctl_c *light_c);

struct pvr_colorbar_ctl_c * init_colorbar_ctl_c(void);
void dealloc_colorbar_ctl_c(struct pvr_colorbar_ctl_c *cbar_c);


struct pvr_colormap_bar_ctl_c * init_colormap_colorbar_ctl_c(void);
void dealloc_colormap_colorbar_ctl_c(struct pvr_colormap_bar_ctl_c *cmap_cbar_c);

#endif /* t_ctl_data_pvr_colormap_c_h_ */
