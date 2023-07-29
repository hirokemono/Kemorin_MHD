/*
//  t_control_data_4_pvr_movie_c.h
//  
//
//  Created by Hiroaki Matsui on 2020/06/03.
*/

#ifndef T_CONTROL_DATA_4_PVR_MOVIE_C_
#define T_CONTROL_DATA_4_PVR_MOVIE_C_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_c_lists.h"
#include "t_control_int_IO.h"
#include "t_control_chara_IO.h"
#include "t_control_real2_IO.h"
#include "t_ctl_data_4_view_transfer_c.h"

struct pvr_movie_ctl_c{
    void * f_self;
    int * f_iflag;
    char * c_block_name;
    
	struct chara_ctl_item *f_movie_mode_ctl;
	struct int_ctl_item *f_num_frames_ctl;
	
	struct chara_ctl_item *f_rotation_axis_ctl;
	
	struct modelview_ctl_c *f_view_start_ctl;
	struct modelview_ctl_c *f_view_end_ctl;
    struct void_clist *f_mul_mmats_c;

    struct real2_ctl_item *f_angle_range_ctl;
	struct real2_ctl_item *f_apature_range_ctl;
	struct real2_ctl_item *f_LIC_kernel_peak_range_ctl;
};

/* prototypes */

struct pvr_movie_ctl_c * init_pvr_movie_ctl_c();

void dealloc_f_PVR_mul_vmats_ctls(struct void_clist *f_vmat_ctls);
void dealloc_pvr_movie_ctl_c(struct pvr_movie_ctl_c *movie_c);

int write_lic_movie_ctl_c(FILE *fp, int level, const char *label,
						  struct pvr_movie_ctl_c *movie_c);

#endif /* T_CONTROL_DATA_4_PVR_MOVIE_C_ */
