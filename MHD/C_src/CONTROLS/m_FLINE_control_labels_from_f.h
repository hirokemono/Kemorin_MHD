/*
//  m_FLINE_control_labels_from_f.h
//
//  Created by Hiroaki Matsui on 05/07/20.
//
*/
/*
Labels for Fieldline module controls

Check fline_ctl_lbls->label_fline_ctl
ctl_list->num_labels 14 
label[0]   fline_file_head 
label[1]   fline_output_type 
label[2]   chosen_ele_grp_ctl 
label[3]   field_line_field_ctl 
label[4]   coloring_field_ctl 
label[5]   coloring_comp_ctl 
label[6]   num_fieldline_ctl 
label[7]   line_direction_ctl 
label[8]   max_line_stepping_ctl 
label[9]   starting_type_ctl 
label[10]   selection_type_ctl 
label[11]   start_surf_grp_ctl 
label[12]   starting_point_ctl 
label[13]   starting_gl_surface_id 

Check fline_ctl_lbls->fline_start_flags
ctl_list->num_labels 4 
label[0]   surface_group 
label[1]   surface_list 
label[2]   position_list 
label[3]   spray_in_domain 

Check fline_ctl_lbls->fline_direction_flags
ctl_list->num_labels 3 
label[0]   forward 
label[1]   backward 
label[2]   both 

Check fline_ctl_lbls->fline_seeds_flags
ctl_list->num_labels 3 
label[0]   amplitude 
label[1]   area_size 
label[2]   no_random 
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "skip_comment_c.h"
#include "t_control_label_from_f.h"

#ifndef M_FLINE_CONTROL_LABELS_FROM_F_
#define M_FLINE_CONTROL_LABELS_FROM_F_

struct fline_control_labels{
	struct control_labels_f *label_fline_ctl;
	
	struct control_labels_f *fline_start_flags;
	struct control_labels_f *fline_direction_flags;
	struct control_labels_f *fline_seeds_flags;
};

/*  prototype */

struct fline_control_labels * init_fline_control_labels();
void dealloc_fline_control_labels(struct fline_control_labels *fline_ctl_lbls);
void check_fline_control_labels(struct fline_control_labels *fline_ctl_lbls);

#endif    /* M_FLINE_CONTROL_LABELS_FROM_F_ */
