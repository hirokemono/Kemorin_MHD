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
#include "t_control_chara_IO.h"
#include "t_control_chara_real_IO.h"
#include "t_control_chara2_IO.h"

struct psf_define_ctl_c{
	int maxlen;
	
	struct chara_ctl_item *section_method_ctl;
	
	struct chara_real_ctl_list psf_coefs_list;
	struct chara_real_ctl_list psf_normal_list;
	struct chara_real_ctl_list psf_center_list;
	struct chara_real_ctl_list psf_axis_list;
	
	struct real_ctl_item *radius_psf_ctl;
	struct chara_ctl_item *psf_group_name_ctl;
	
	struct chara_ctl_list psf_area_list;
};

struct psf_field_ctl_c{
	int maxlen;
	
	struct chara2_ctl_list psf_out_field_list;
};

struct psf_ctl_c{
	int maxlen;
	
	struct chara_ctl_item *psf_file_head_ctl;
	struct chara_ctl_item *psf_output_type_ctl;
	
	int iflag_surface_define;
    char *psf_def_file_name;
	struct psf_define_ctl_c *psf_def_c;
	int iflag_output_field;
	struct psf_field_ctl_c *psf_fld_c;
};


/* prototypes */

void get_label_psf_define_ctl(int index, char *label);
void get_label_psf_field_ctl(int index, char *label);
void get_label_psf_ctl(int index, char *label);

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


int read_psf_define_file_c(const char *file_name, char buf[LENGTHBUF],
			struct psf_define_ctl_c *psf_def_c);
int write_psf_define_file_c(const char *file_name, struct psf_define_ctl_c *psf_def_c);

void rename_psf_define_file_c(struct psf_ctl_c *psf_c);
int read_psf_ctl_file_c(const char *file_name, char buf[LENGTHBUF],
			struct psf_ctl_c *psf_c);
int write_psf_ctl_file_c(const char *file_name, struct psf_ctl_c *psf_c);

#endif /* t_control_data_4_psf_c_h_ */
