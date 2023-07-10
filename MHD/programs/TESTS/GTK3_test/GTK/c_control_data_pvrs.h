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


struct f_VIZ_PVR_ctl{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
	
    char *pvr_ctl_file_name;
	
	char *f_fname_mat_ctl;
	void *f_mat;
	char *f_fname_pvr_light_c;
	void *f_light;
	char *f_fname_cmap_cbar_c;
	void *f_cmap_cbar_c;
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
};


/* prototypes */


struct f_VIZ_PVR_ctl * init_f_VIZ_PVR_ctl(int idx, void *f_parent);
void *dealloc_f_VIZ_PVR_ctl(void *block_item);


#endif /* C_CONTROL_DATA_PVRS_H_ */
