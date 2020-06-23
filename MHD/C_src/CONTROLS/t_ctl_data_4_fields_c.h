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
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_chara_IO.h"
#include "t_control_chara3_IO.h"
#include "t_control_chara_int2_IO.h"

#include "m_field_name_from_f.h"

struct field_ctl_c{
    int iflag_use;
	int maxlen;
	
	struct chara3_ctl_item *tmp_fld_item;
	struct chara_int2_ctl_list field_list;
	
	struct chara_ctl_list quad_phys_list;
    struct chara_ctl_list viz_comp_list;
};

struct all_field_ctl_c{
	struct field_names_f *fld_list;
	
	int *iflag_use;
	int *iflag_viz;
	int *iflag_monitor;
    int *iflag_quad;
};

/* prototype */
void get_label_field_ctl(int index, char *label);

struct field_ctl_c * init_field_ctl_c();
void dealloc_field_ctl_c(struct field_ctl_c *fld_ctl);
void read_field_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct field_ctl_c *fld_ctl);
int write_field_ctl_c(FILE *fp, int level, const char *label, struct field_ctl_c *fld_ctl);


struct all_field_ctl_c * init_all_field_ctl_c();
void dealloc_all_field_ctl_c(struct all_field_ctl_c *all_fld_list);

void add_field_wqflag_to_ctl(int i_fld, struct all_field_ctl_c *all_fld_list, 
			struct field_ctl_c *fld_ctl);
void delete_field_wqflag_in_ctl(int i_fld, struct all_field_ctl_c *all_fld_list,
			struct field_ctl_c *fld_ctl);
void update_field_flag_wqflag_in_ctl(int i_fld, struct all_field_ctl_c *all_fld_list, 
			struct field_ctl_c *fld_ctl);

void load_field_w_qflag_from_ctl(struct field_ctl_c *fld_ctl, 
			struct all_field_ctl_c *all_fld_list);
void load_field_w_qflag_to_ctl(struct all_field_ctl_c *all_fld_list, 
			struct field_ctl_c *fld_ctl);
void reflesh_field_ctl_list(struct all_field_ctl_c *all_fld_list, 
			struct field_ctl_c *fld_ctl);

void check_field_ctl_list(struct field_ctl_c *fld_ctl);
void check_field_and_comp_ctl_list(struct field_ctl_c *fld_ctl);

#endif /* t_ctl_data_4_fields_c_h_ */
