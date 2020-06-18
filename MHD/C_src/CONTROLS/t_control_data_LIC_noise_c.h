/*
//  t_control_data_LIC_noise_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/05.
*/

#ifndef t_control_data_LIC_noise_c_h_
#define t_control_data_LIC_noise_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"
#include "m_LIC_control_labels_from_f.h"

struct lic_noise_ctl_c{
	struct control_labels_f *label_lic_noise;
	
	struct chara_ctl_item *noise_type_ctl_c;
	struct chara_ctl_item *noise_file_name_ctl_c;
	struct chara_ctl_item *noise_file_format_ctl_c;
	
	struct int_ctl_item *noise_resolution_ctl_c;
	struct int_ctl_item *noise_stepping_ctl_c;
	
	struct real_ctl_item *noise_cube_size_ctl_c;
};

/* prototypes */
struct lic_noise_ctl_c * init_lic_noise_ctl_c();
void dealloc_lic_noise_ctl_c(struct lic_noise_ctl_c *lic_nze_c);
int read_lic_noise_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct lic_noise_ctl_c *lic_nze_c);
int write_lic_noise_ctl_c(FILE *fp, int level, const char *label, 
			struct lic_noise_ctl_c *lic_nze_c);


#endif /* t_control_data_LIC_noise_c_h_ */
