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
#include "t_control_int_IO.h"
#include "t_control_chara_IO.h"
#include "t_control_real2_IO.h"
#include "t_ctl_data_4_view_transfer_c.h"
#include "m_PVR_control_labels_from_f.h"

struct pvr_movie_ctl_c{
	int iflag_use;
	struct control_labels_f *label_lic_movie_ctl;
	
	struct chara_ctl_item *movie_mode_ctl;
	struct int_ctl_item *num_frames_ctl;
	
	struct chara_ctl_item *rotation_axis_ctl;
	
	char *start_view_file_name;
	struct modelview_ctl_c *view_start_mat_c;
    
	char *end_view_file_name;
	struct modelview_ctl_c *view_end_mat_c;
    
	struct real2_ctl_item *apature_range_ctl;
	struct real2_ctl_item *LIC_kernel_peak_range_ctl;
};

/* prototypes */

struct pvr_movie_ctl_c * init_pvr_movie_ctl_c();
void dealloc_pvr_movie_ctl_c(struct pvr_movie_ctl_c *movie_c);
void read_pvr_movie_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
						  struct pvr_movie_ctl_c *movie_c);
int write_pvr_movie_ctl_c(FILE *fp, int level, const char *label,
						  struct pvr_movie_ctl_c *movie_c);
int write_lic_movie_ctl_c(FILE *fp, int level, const char *label,
						  struct pvr_movie_ctl_c *movie_c);

#endif /* T_CONTROL_DATA_4_PVR_MOVIE_C_ */
