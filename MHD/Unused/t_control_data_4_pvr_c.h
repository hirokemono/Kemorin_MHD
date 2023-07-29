/*
//  t_control_data_4_pvr_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#ifndef t_control_data_4_pvr_c_h_
#define t_control_data_4_pvr_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_int_IO.h"
#include "t_control_chara_IO.h"
#include "t_control_chara2_real_IO.h"
#include "t_ctl_data_pvr_colormap_c.h"
#include "t_ctl_data_4_view_transfer_c.h"
#include "t_control_data_4_pvr_movie_c.h"
#include "t_control_data_pvr_section_list.h"
#include "t_control_data_pvr_isosurf_list.h"
#include "m_PVR_control_labels_from_f.h"
#include "ctl_data_pvr_colormap_IO_c.h"


struct pvr_plot_area_ctl_c{
	void * f_self;
	int * f_iflag;
	char * c_block_name;
	
	struct chara_clist       *f_pvr_area_ctl;
	struct chara2_real_clist *f_surf_enhanse_ctl;
};

struct pvr_ctl_c{
	void * f_self;
	int * f_iflag;
	char * c_block_name;
	
	struct control_labels_f *label_pvr_ctl_w_dpl;
	
	char *color_file_ctl;
	
	struct chara_ctl_item *updated_ctl;
	
	struct chara_ctl_item *file_head_ctl;
	struct chara_ctl_item *file_fmt_ctl;
	
	struct chara_ctl_item *monitoring_ctl;
	struct chara_ctl_item *streo_ctl;
    struct chara_ctl_item *anaglyph_ctl;
	struct chara_ctl_item *quilt_ctl;
	
	struct chara_ctl_item *pvr_field_ctl;
	struct chara_ctl_item *pvr_comp_ctl;

	struct pvr_plot_area_ctl_c *area_c;
    
	char *pvr_modelview_file_name;
	struct modelview_ctl_c *mat_c;
    
	struct lighting_ctl_c *light_c;
	
	char *pvr_colormap_file_name;
	int iflag_cmap_cbar_ctl;
	struct pvr_colormap_bar_ctl_c *cmap_cbar_c;
	
	struct pvr_movie_ctl_c *movie_c;
	struct pvr_sect_ctl_list pvr_sect_c_list;
	struct pvr_iso_ctl_list  pvr_iso_c_list;
};

/* prototypes */

void dealloc_pvr_plot_area_ctl_c(struct pvr_plot_area_ctl_c *area_c);

struct pvr_plot_area_ctl_c * init_pvr_plot_area_ctl_c();
void dealloc_pvr_plot_area_ctl_c(struct pvr_plot_area_ctl_c *area_c);

struct pvr_ctl_c * init_pvr_ctl_c();
void dealloc_pvr_ctl_c(struct pvr_ctl_c *pvr_c);

#endif /* t_control_data_4_pvr_c_h_ */
