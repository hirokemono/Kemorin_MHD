/*
//  t_control_data_LIC_kernel_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/05.
*/

#ifndef t_control_data_LIC_kernel_c_h_
#define t_control_data_LIC_kernel_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"
#include "m_LIC_control_labels_from_f.h"

struct lic_kernel_ctl_c{
	struct control_labels_f *label_lic_kernel;
	
	struct chara_ctl_item *kernel_function_type_ctl;
	struct int_ctl_item *kernel_resolution_ctl_c;
	
	struct real_ctl_item *kernel_sigma_ctl_c;
	struct real_ctl_item *kernel_peak_ctl_c;
	struct real_ctl_item *kernel_half_lengh_c;
	
	struct chara_ctl_item *LIC_trace_length_def_ctl;
	struct int_ctl_item *LIC_trace_count_ctl;
};

/* prototypes */
struct lic_kernel_ctl_c * init_lic_kernel_ctl_c();
void dealloc_lic_kernel_ctl_c(struct lic_kernel_ctl_c *lic_knl_c);
int read_lic_kernel_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct lic_kernel_ctl_c *lic_knl_c);
int write_lic_kernel_ctl_c(FILE *fp, int level, const char *label, 
			struct lic_kernel_ctl_c *lic_knl_c);


#endif /* t_control_data_LIC_kernel_c_h_ */
