/*
//  t_control_data_4_iso_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#ifndef t_control_data_4_iso_c_h_
#define t_control_data_4_iso_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "control_arrays_IO_c.h"

struct iso_define_ctl_c{
	int maxlen;
	
	struct chara_ctl_item *isosurf_data_ctl;
	struct chara_ctl_item *isosurf_comp_ctl;
	
	struct real_ctl_item *isosurf_value_ctl;
	
	struct chara_ctl_array *iso_area_ctl;
};

struct iso_field_ctl_c{
	int maxlen;
	
	struct chara_ctl_item *iso_result_type_ctl;
	struct real_ctl_item *result_value_iso_ctl;
	
	struct chara2_ctl_array *iso_out_field_ctl;
};

struct iso_ctl_c{
	int maxlen;
	
	struct chara_ctl_item *iso_file_head_ctl;
	struct chara_ctl_item *iso_output_type_ctl;
	
	int iflag_isosurf_define;
	struct iso_define_ctl_c *iso_def_c;
	int iflag_iso_output_field;
	struct iso_field_ctl_c *iso_fld_c;
};

/* prototypes */

void alloc_iso_define_ctl_c(struct iso_define_ctl_c *iso_def_c);
void dealloc_iso_define_ctl_c(struct iso_define_ctl_c *iso_def_c);
int read_iso_area_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
                        struct iso_define_ctl_c *iso_def_c);
int read_iso_define_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct iso_define_ctl_c *iso_def_c);
int write_iso_define_ctl_c(FILE *fp, int level, const char *label, 
			struct iso_define_ctl_c *iso_def_c);

void alloc_iso_field_ctl_c(struct iso_field_ctl_c *iso_fld_c);
void dealloc_iso_field_ctl_c(struct iso_field_ctl_c *iso_fld_c);
int read_iso_field_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct iso_field_ctl_c *iso_fld_c);
int write_iso_field_ctl_c(FILE *fp, int level, const char *label, 
			struct iso_field_ctl_c *iso_fld_c);

void alloc_iso_ctl_c(struct iso_ctl_c *iso_c);
void dealloc_iso_ctl_c(struct iso_ctl_c *iso_c);
int read_psf_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct iso_ctl_c *iso_c);
int write_iso_ctl_c(FILE *fp, int level, const char *label, 
			struct iso_ctl_c *iso_c);

int read_iso_ctl_file_c(const char *file_name, char buf[LENGTHBUF],
                        struct iso_ctl_c *iso_c);
int write_iso_ctl_file_c(const char *file_name, struct iso_ctl_c *iso_c);


#endif /* t_control_data_4_iso_c_h_ */
