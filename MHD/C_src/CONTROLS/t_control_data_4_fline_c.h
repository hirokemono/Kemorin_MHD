/*
//  t_control_data_4_fline_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#ifndef t_control_data_4_fline_c_h_
#define t_control_data_4_fline_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "control_lists_IO_c.h"
#include "control_arrays_IO_c.h"

struct fline_ctl_c{
	int maxlen;
	
	struct chara_ctl_item *fline_file_head_ctl;
	struct chara_ctl_item *fline_output_type_ctl;
	
	struct chara_ctl_item *fline_field_ctl;
	struct chara_ctl_item *fline_color_field_ctl;
	struct chara_ctl_item *fline_color_comp_ctl;
	
	struct chara_ctl_array *fline_area_grp_ctl;
	
	struct chara_ctl_item *starting_type_ctl;
	struct chara_ctl_item *selection_type_ctl;
	struct chara_ctl_item *line_direction_ctl;
	
	struct chara_ctl_item *start_surf_grp_ctl;
	struct int_ctl_item *num_fieldline_ctl;
	struct int_ctl_item *max_line_stepping_ctl;
	
	struct real3_ctl_list seed_point_list;
	struct int2_ctl_array *seed_surface_ctl;
};

/* prototypes */

void get_label_fline_ctl(int index, char *label);

void alloc_fline_ctl_c(struct fline_ctl_c *fline_c);
void dealloc_fline_ctl_c(struct fline_ctl_c *fline_c);
int read_fline_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct fline_ctl_c *fline_c);
int write_fline_ctl_c(FILE *fp, int level, const char *label, 
			struct fline_ctl_c *fline_c);

int read_fline_ctl_file_c(const char *file_name, char buf[LENGTHBUF],
			struct fline_ctl_c *fline_c);
int write_fline_ctl_file_c(const char *file_name, struct fline_ctl_c *fline_c);

#endif /* t_control_data_4_fline_c_h_ */
