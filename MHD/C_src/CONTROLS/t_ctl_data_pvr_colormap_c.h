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
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "control_arrays_IO_c.h"

struct colormap_ctl_c{
	int maxlen;
	
	struct chara_ctl_item *colormap_mode_ctl;
	
	struct chara_ctl_item *lic_color_fld_ctl;
	struct chara_ctl_item *lic_color_comp_ctl;
	struct chara_ctl_item *lic_opacity_fld_ctl;
	struct chara_ctl_item *lic_opacity_comp_ctl;
	
	struct chara_ctl_item *data_mapping_ctl;
	struct real2_ctl_array *colortbl_ctl;
	
	struct chara_ctl_item *opacity_style_ctl;
	struct real_ctl_item *fix_opacity_ctl;
	struct real2_ctl_array *linear_opacity_ctl;
	struct real3_ctl_array *step_opacity_ctl;
	
	struct real_ctl_item *range_min_ctl;
	struct real_ctl_item *range_max_ctl;
};

struct lighting_ctl_c{
	int maxlen;
	
	struct real_ctl_item *ambient_coef_ctl;
	struct real_ctl_item *diffuse_coef_ctl;
	struct real_ctl_item *specular_coef_ctl;
	
	struct real3_ctl_array *light_position_ctl;
};

struct pvr_colormap_ctl_c{
	int maxlen;
	
	int iflag_colormap_ctl;
	struct colormap_ctl_c *cmap_c;
	int iflag_lighting_ctl;
	struct lighting_ctl_c *light_c;
};

/* prototypes */
void get_label_colormap_ctl(int index, char *label);
void get_label_lighting_ctl(int index, char *label);

void alloc_colormap_ctl_c(struct colormap_ctl_c *cmap_c);
void dealloc_colormap_ctl_c(struct colormap_ctl_c *cmap_c);
int read_colormap_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct colormap_ctl_c *cmap_c);
int write_colormap_ctl_c(FILE *fp, int level, const char *label, 
			struct colormap_ctl_c *cmap_c);

void alloc_lighting_ctl_c(struct lighting_ctl_c *light_c);
void dealloc_lighting_ctl_c(struct lighting_ctl_c *light_c);
int read_lighting_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct lighting_ctl_c *light_c);
int write_lighting_ctl_c(FILE *fp, int level, const char *label, 
			struct lighting_ctl_c *light_c);

int read_colormap_file_c(const char *file_name, char buf[LENGTHBUF],
			struct colormap_ctl_c *cmap_c);
int write_colormap_file_c(const char *file_name, struct colormap_ctl_c *cmap_c);

#endif /* t_ctl_data_pvr_colormap_c_h_ */
