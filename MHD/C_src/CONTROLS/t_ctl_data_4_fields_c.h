/*
//  t_ctl_data_4_fields_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/02.
*/

#ifndef t_ctl_data_4_fields_c_h_
#define t_ctl_data_4_fields_c_h_

#define NLBL_FIELD_CTL 3

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_chara_IO.h"
#include "t_control_chara3_IO.h"
#include "t_control_chara_int2_IO.h"

#include "all_field_names_c.h"


struct field_ctl_c{
	int maxlen;
	
	struct chara3_ctl_item *tmp_fld_item;
	struct chara_int2_ctl_list field_list;
	
	struct chara_ctl_list quad_phys_list;
};

struct all_field_ctl_c{
	
	char field_name[NCHARA_FIELD];
	char field_math[KCHARA_C];
	int num_comp;
	int iflag_use;
	int iflag_viz;
	int iflag_monitor;
    int iflag_quad;
};

/* prototype */
void get_label_field_ctl(int index, char *label);

void alloc_field_ctl_c(struct field_ctl_c *fld_ctl);
void dealloc_field_ctl_c(struct field_ctl_c *fld_ctl);
int read_field_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct field_ctl_c *fld_ctl);
int write_field_ctl_c(FILE *fp, int level, const char *label, struct field_ctl_c *fld_ctl);


void alloc_all_field_ctl_c(struct all_field_ctl_c **all_fld_tbl);
void dealloc_all_field_ctl_c(struct all_field_ctl_c **all_fld_tbl);

void add_field_wqflag_to_ctl(struct all_field_ctl_c *all_fld_tbl, 
			struct field_ctl_c *fld_ctl);
void delete_field_wqflag_in_ctl(struct all_field_ctl_c *all_fld_tbl,
			struct field_ctl_c *fld_ctl);
void update_field_flag_wqflag_in_ctl(struct all_field_ctl_c *all_fld_tbl, 
			struct field_ctl_c *fld_ctl);

void load_field_w_qflag_from_ctl(struct field_ctl_c *fld_ctl, 
			struct all_field_ctl_c **all_fld_tbl);
void load_field_w_qflag_to_ctl(struct all_field_ctl_c **all_fld_tbl, 
			struct field_ctl_c *fld_ctl);
void reflesh_field_ctl_list(struct all_field_ctl_c **all_fld_tbl, 
			struct field_ctl_c *fld_ctl);

void check_field_ctl_list(struct field_ctl_c *fld_ctl);

#endif /* t_ctl_data_4_fields_c_h_ */
