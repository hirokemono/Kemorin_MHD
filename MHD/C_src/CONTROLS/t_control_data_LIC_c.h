/*
//  t_control_data_LIC_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/05.
*/

#ifndef t_control_data_LIC_c_h_
#define t_control_data_LIC_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "control_arrays_IO_c.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"
#include "t_control_data_lic_masking_list.h"


struct lic_ctl_c{
	int maxlen;
	
	struct chara_ctl_item *LIC_field_ctl;
	
	struct chara_ctl_item *color_field_ctl;
	struct chara_ctl_item *color_component_ctl;
	struct chara_ctl_item *opacity_field_ctl;
	struct chara_ctl_item *opacity_component_ctl;
	
	struct chara_ctl_item *noise_type_ctl;
	struct chara_ctl_item *noise_file_prefix_ctl;
	struct int_ctl_item *noise_resolution_ctl;
	
	struct chara_ctl_item *kernel_function_type_ctl;
	struct chara_ctl_item *kernal_file_prefix_ctl;
	
	struct chara_ctl_item *LIC_trace_length_def_ctl;
	struct real_ctl_item *LIC_trace_length_ctl;
	struct int_ctl_item *LIC_trace_count_ctl;
	
	struct chara_ctl_item *normalization_type_ctl;
	struct real_ctl_item *normalization_value_ctl;
	
	struct chara_ctl_item *reflection_ref_type_ctl;
	struct real_ctl_item *reflection_parameter_ctl;
	
	struct lic_masking_ctl_list lic_mask_list;
};

/* prototypes */
void get_label_lic_ctl_c(int index, char *label);

void alloc_lic_ctl_c(struct lic_ctl_c *lic_c);
void dealloc_lic_ctl_c(struct lic_ctl_c *lic_c);
int read_lic_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct lic_ctl_c *lic_c);
int write_lic_ctl_c(FILE *fp, int level, const char *label, 
			struct lic_ctl_c *lic_c);


#endif /* t_control_data_LIC_c_h_ */
