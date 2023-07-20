/*
//  c_control_data_pvrs.h
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#ifndef C_CONTROL_DATA_PVRS_H_
#define C_CONTROL_DATA_PVRS_H_

#include <stdio.h>
#include <stdlib.h> 
#include <string.h>

#include "skip_comment_c.h"
#include "t_control_c_lists.h"
#include "t_control_chara_IO.h"
#include "t_ctl_array_single_items_c.h"
#include "t_ctl_data_pvr_colormap_c.h"
#include "c_ctl_data_PVR_colormap.h"
#include "c_ctl_data_PVR_view_matrix.h"


struct f_VIZ_PVR_ctl{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
    char *pvr_ctl_file_name;
	
    struct modelview_ctl_c *f_mat;
	struct lighting_ctl_c *f_light;
	struct pvr_colormap_bar_ctl_c *f_cmap_cbar_c;
	void *f_movie;
	void *f_quilt_c;
    
	struct chara_ctl_item *f_updated_ctl;
	struct chara_ctl_item *f_file_head_ctl;
	struct chara_ctl_item *f_file_fmt_ctl;
	struct chara_ctl_item *f_monitoring_ctl;
	struct chara_ctl_item *f_streo_ctl;
	struct chara_ctl_item *f_anaglyph_ctl;
	struct chara_ctl_item *f_quilt_ctl;
	void *f_render_area_c;
	struct chara_ctl_item *f_pvr_field_ctl;
	struct chara_ctl_item *f_pvr_comp_ctl;
	void *f_pvr_scts_c;
	void *f_pvr_isos_c;
    
    void *void_panel;
};


/* prototypes */


struct f_VIZ_PVR_ctl * init_f_VIZ_PVR_ctl(void *(*c_load_self)(int idx, void *f_parent),
                                          int idx, void *f_parent);
void *dealloc_f_VIZ_PVR_ctl(void *block_item);


#endif /* C_CONTROL_DATA_PVRS_H_ */
