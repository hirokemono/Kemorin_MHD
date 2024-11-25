/*
//  c_ctl_data_FLINE.h
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#ifndef C_CTL_DATA_FLINE_H_
#define C_CTL_DATA_FLINE_H_

#include <stdio.h>
#include <stdlib.h> 
#include <string.h>

#include "skip_comment_c.h"
#include "t_control_c_lists.h"
#include "t_control_c_lists.h"
#include "t_control_c_lists.h"
#include "t_control_chara_IO.h"
#include "t_control_int_IO.h"
#include "t_control_real3_IO.h"
#include "t_control_int2_IO.h"
#include "t_ctl_array_single_items_c.h"
#include "t_ctl_array_real3_items_c.h"
#include "t_ctl_array_int2_items_c.h"


struct f_VIZ_FLINE_ctl{
	void * f_self;
	int * f_iflag;
	
	char * c_block_name;
    char *fline_ctl_file_name;
	
    struct chara_ctl_item *f_fline_file_head_ctl;
    struct chara_ctl_item  *f_fline_output_type_ctl;
    struct chara_ctl_item  *f_fline_field_ctl;
    struct chara_ctl_item  *f_fline_color_field_ctl;
    struct chara_ctl_item  *f_fline_color_comp_ctl;
    struct chara2_ctl_item *f_fline_field_output_ctl;
    struct chara_clist     *f_fline_area_grp_ctl;
    struct chara_ctl_item  *f_fline_comm_mode_ctl;
    struct chara_ctl_item  *f_starting_type_ctl;
    struct chara_ctl_item  *f_selection_type_ctl;
    struct chara_ctl_item  *f_line_direction_ctl;
    struct chara_ctl_item  *f_start_surf_grp_ctl;
    struct int_ctl_item    *f_num_fieldline_ctl;
    struct int_ctl_item    *f_max_line_stepping_ctl;
    struct int_ctl_item    *f_max_trace_length_ctl;
    struct real3_clist     *f_seed_point_ctl;
    struct int2_clist      *f_seed_surface_ctl;
    
    void *void_panel;
};


/* prototypes */

extern void * c_append_viz_fline_ctls(int idx, char *block_name, void *f_fline_ctls);
extern void * c_delete_viz_fline_ctls(int idx, void *f_fline_ctls);

struct f_VIZ_FLINE_ctl * init_f_VIZ_FLINE_ctl(int idx, void *f_parent);
void dealloc_f_VIZ_FLINE_ctl(struct f_VIZ_FLINE_ctl *f_fline_ctl);

struct void_clist * init_f_VIZ_fline_ctls(void *f_parent, int *f_num_fline_ctl);

#endif /* C_CTL_DATA_FLINE_H_ */
