/*
//  t_control_data_4_psf_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#ifndef t_control_data_4_psf_c_h_
#define t_control_data_4_psf_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "control_arrays_IO_c.h"

struct psf_define_ctl_c{
	int maxlen;
	
	struct chara_ctl_item *section_method_ctl;
	
	struct chara_real_ctl_array *psf_coefs_ctl;
	struct chara_real_ctl_array *psf_normal_ctl;
	struct chara_real_ctl_array *psf_center_ctl;
	struct chara_real_ctl_array *psf_axis_ctl;
	
	struct real_ctl_item *radius_psf_ctl;
	struct chara_ctl_item *psf_group_name_ctl;
	
	struct chara_ctl_array *psf_area_ctl;
};

struct psf_field_ctl_c{
	int maxlen;
	
	struct chara2_ctl_array *psf_out_field_ctl;
};

struct psf_ctl_c{
	int maxlen;
	
	struct chara_ctl_item *psf_file_head_ctl;
	struct chara_ctl_item *psf_output_type_ctl;
	
	int iflag_surface_define;
	struct psf_define_ctl_c *psf_def_c;
	int iflag_output_field;
	struct psf_field_ctl_c *psf_fld_c;
};


/* prototypes */

void alloc_psf_define_ctl_c(struct psf_define_ctl_c *psf_def_c);
void dealloc_psf_define_ctl_c(struct psf_define_ctl_c *psf_def_c);
int read_psf_define_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct psf_define_ctl_c *psf_def_c);
int write_psf_define_ctl_c(FILE *fp, int level, const char *label, 
			struct psf_define_ctl_c *psf_def_c);

void alloc_psf_field_ctl_c(struct psf_field_ctl_c *psf_fld_c);
void dealloc_psf_field_ctl_c(struct psf_field_ctl_c *psf_fld_c);
int read_psf_field_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct psf_field_ctl_c *psf_fld_c);
int write_psf_field_ctl_c(FILE *fp, int level, const char *label, 
			struct psf_field_ctl_c *psf_fld_c);

void alloc_psf_ctl_c(struct psf_ctl_c *psf_c);
void dealloc_psf_ctl_c(struct psf_ctl_c *psf_c);
int read_psf_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct psf_ctl_c *psf_c);
int write_psf_ctl_c(FILE *fp, int level, const char *label, 
			struct psf_ctl_c *psf_c);

#endif /* t_control_data_4_psf_c_h_ */
