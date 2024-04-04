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
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_int_IO.h"
#include "t_control_chara_IO.h"
#include "t_control_int2_IO.h"
#include "t_control_real3_IO.h"
#include "m_FLINE_control_labels_from_f.h"

struct fline_ctl_c{
	struct fline_control_labels *fline_ctl_lbls;
	
	struct chara_ctl_item *fline_file_head_ctl;
	struct chara_ctl_item *fline_output_type_ctl;
	
	struct chara_ctl_item *fline_field_ctl;
	struct chara_ctl_item *fline_color_field_ctl;
	struct chara_ctl_item *fline_color_comp_ctl;
	
    struct chara_clist *fline_area_grp_list;
	
	struct chara_ctl_item *starting_type_ctl;
	struct chara_ctl_item *selection_type_ctl;
	struct chara_ctl_item *line_direction_ctl;
	
	struct chara_ctl_item *start_surf_grp_ctl;
	struct int_ctl_item *num_fieldline_ctl;
	struct int_ctl_item *max_line_stepping_ctl;
	
	struct real3_clist *seed_point_list;
	struct int2_clist *seed_surface_list;
};

/* prototypes */
struct fline_ctl_c * init_fline_ctl_c();
void dealloc_fline_ctl_c(struct fline_ctl_c *fline_c);
int read_fline_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct fline_ctl_c *fline_c);
int write_fline_ctl_c(FILE *fp, int level, const char *label, 
			struct fline_ctl_c *fline_c);

int read_fline_ctl_file_c(const char *file_name, char buf[LENGTHBUF],
			struct fline_ctl_c *fline_c);
int write_fline_ctl_file_c(const char *file_name, struct fline_ctl_c *fline_c);

#endif /* t_control_data_4_fline_c_h_ */
