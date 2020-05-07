/*
//  t_ctl_data_4_kemoview_mesh_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/02.
*/

#ifndef T_CTL_DATA_4_KEMOVIEW_MESH_C_
#define T_CTL_DATA_4_KEMOVIEW_MESH_C_

#define NLBL_FIELD_CTL 3

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_chara_int_IO.h"
#include "t_control_chara_int3_IO.h"

struct kemoview_mesh_ctl_c{
    int iflag_use;
	int maxlen;
	
	struct chara_int3_ctl_list mesh_domain_list;
	struct chara_int_ctl_list  mesh_node_group_list;
	struct chara_int3_ctl_list mesh_ele_group_list;
	struct chara_int3_ctl_list mesh_surf_group_list;
};

/* prototype */
void get_label_field_ctl(int index, char *label);

void alloc_field_ctl_c(struct field_ctl_c *fld_ctl);
void dealloc_field_ctl_c(struct field_ctl_c *fld_ctl);
void read_field_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct field_ctl_c *fld_ctl);
int write_field_ctl_c(FILE *fp, int level, const char *label, struct field_ctl_c *fld_ctl);

void check_field_ctl_list(struct field_ctl_c *fld_ctl);

#endif /* t_ctl_data_4_kemoview_mesh_c */
