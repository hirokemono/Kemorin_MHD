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

#define NLBL_COLORMAP_CTL     13
#define NLBL_LIGHTING_CTL      4
#define NLBL_PVR_COLORBAR_CTL  7
#define NLBL_CMAP_CBAR_CTL     2

struct colormap_ctl_c{
    int iflag_use;
	int maxlen;
	
	struct chara_ctl_item *colormap_mode_ctl;
	
	struct chara_ctl_item *lic_color_fld_ctl;
	struct chara_ctl_item *lic_color_comp_ctl;
	struct chara_ctl_item *lic_opacity_fld_ctl;
	struct chara_ctl_item *lic_opacity_comp_ctl;
	
	struct chara_ctl_item *data_mapping_ctl;
	struct real2_clist *colortbl_list;
	
	struct chara_ctl_item *opacity_style_ctl;
	struct real_ctl_item *fix_opacity_ctl;
	struct real2_clist *linear_opacity_list;
	struct real3_clist *step_opacity_list;
	
	struct real_ctl_item *range_min_ctl;
	struct real_ctl_item *range_max_ctl;
};

struct lighting_ctl_c{
    int iflag_use;
	int maxlen;
	
	struct real_ctl_item *ambient_coef_ctl;
	struct real_ctl_item *diffuse_coef_ctl;
	struct real_ctl_item *specular_coef_ctl;
	
	struct real3_clist *light_position_list;
};

struct pvr_colorbar_ctl_c{
    int iflag_use;
	int maxlen;
	
	struct chara_ctl_item *colorbar_switch_ctl;
	struct chara_ctl_item *colorbar_scale_ctl;
	struct chara_ctl_item *zeromarker_flag_ctl;
	
	struct int_ctl_item *font_size_ctl;
	struct int_ctl_item *ngrid_cbar_ctl;
	
	struct real2_ctl_item *cbar_range_ctl;
	
	struct chara_ctl_item *axis_switch_ctl;
    struct chara_ctl_item *time_switch_ctl;
};

struct pvr_colormap_bar_ctl_c{
    int iflag_use;
	int maxlen;
	
	struct colormap_ctl_c *cmap_c;
	struct pvr_colorbar_ctl_c *cbar_c;
};

/* prototypes */
struct colormap_ctl_c * init_colormap_ctl_c(void);
void dealloc_colormap_ctl_c(struct colormap_ctl_c *cmap_c);
void read_colormap_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct colormap_ctl_c *cmap_c);
int write_colormap_ctl_c(FILE *fp, int level, const char *label, 
			struct colormap_ctl_c *cmap_c);

struct lighting_ctl_c * init_lighting_ctl_c(void);
void dealloc_lighting_ctl_c(struct lighting_ctl_c *light_c);
void read_lighting_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct lighting_ctl_c *light_c);
int write_lighting_ctl_c(FILE *fp, int level, const char *label, 
			struct lighting_ctl_c *light_c);

struct pvr_colorbar_ctl_c * init_colorbar_ctl_c(void);
void dealloc_colorbar_ctl_c(struct pvr_colorbar_ctl_c *cbar_c);
void read_colorbar_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct pvr_colorbar_ctl_c *cbar_c);
int write_colorbar_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_colorbar_ctl_c *cbar_c);


struct pvr_colormap_bar_ctl_c * init_colormap_colorbar_ctl_c(void);
void dealloc_colormap_colorbar_ctl_c(struct pvr_colormap_bar_ctl_c *cmap_cbar_c);
void read_colormap_colorbar_ctl_c(FILE *fp, char buf[LENGTHBUF],
			const char *label, struct pvr_colormap_bar_ctl_c *cmap_cbar_c);
int write_colormap_colorbar_ctl_c(FILE *fp, int level, const char *label,
			struct pvr_colormap_bar_ctl_c *cmap_cbar_c);

void read_colormap_file_c(const char *file_name, char buf[LENGTHBUF],
			struct pvr_colormap_bar_ctl_c *cmap_cbar_c);
void write_colormap_file_c(const char *file_name, 
			struct pvr_colormap_bar_ctl_c *cmap_cbar_c);

#endif /* t_ctl_data_pvr_colormap_c_h_ */
