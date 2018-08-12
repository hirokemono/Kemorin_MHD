/*
//  t_ctl_data_4_fields_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/02.
*/

#ifndef t_ctl_data_4_fields_c_h_
#define t_ctl_data_4_fields_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "control_lists_IO_c.h"
#include "control_arrays_IO_c.h"

struct field_ctl_c{
	int maxlen;
	
	struct chara3_ctl_list field_list;
	
	struct chara_ctl_array *quad_phys_c;
	struct chara_ctl_array *linear_phys_c;
};

/* prototype */
void get_label_field_ctl(int index, char *label);

void alloc_field_ctl_c(struct field_ctl_c *fld_ctl);
void dealloc_field_ctl_c(struct field_ctl_c *fld_ctl);
int read_field_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct field_ctl_c *fld_ctl);
int write_field_ctl_c(FILE *fp, int level, const char *label, struct field_ctl_c *fld_ctl);

#endif /* t_ctl_data_4_fields_c_h_ */
