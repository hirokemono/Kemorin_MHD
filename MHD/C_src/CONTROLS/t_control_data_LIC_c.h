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
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"
#include "t_control_data_lic_masking_list.h"
#include "t_control_data_LIC_noise_c.h"
#include "t_control_data_LIC_kernel_c.h"
#include "m_LIC_control_labels_from_f.h"

struct lic_ctl_c{
	struct control_labels_f *label_lic_ctl;
	
	struct chara_ctl_item *LIC_field_ctl;
	
	struct chara_ctl_item *color_field_ctl;
	struct chara_ctl_item *color_component_ctl;
	struct chara_ctl_item *opacity_field_ctl;
	struct chara_ctl_item *opacity_component_ctl;
	
	struct chara_ctl_item *vr_sample_mode_ctl_c;
	struct real_ctl_item *step_size_ctl_c;
	
	struct chara_ctl_item *normalization_type_ctl_c;
	struct real_ctl_item *normalization_value_ctl_c;
	
	struct lic_masking_ctl_list lic_mask_list;
	struct lic_noise_ctl_c  *lic_nze_c;
	struct lic_kernel_ctl_c *lic_knl_c;
};

/* prototypes */
struct lic_ctl_c * init_lic_ctl_c();
void dealloc_lic_ctl_c(struct lic_ctl_c *lic_c);
int read_lic_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct lic_ctl_c *lic_c);
int write_lic_ctl_c(FILE *fp, int level, const char *label, 
			struct lic_ctl_c *lic_c);


#endif /* t_control_data_LIC_c_h_ */
