/*
//  t_ctl_data_MHD_boundary_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/02.
*/

#ifndef t_ctl_data_MHD_boundary_c_h_
#define t_ctl_data_MHD_boundary_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "control_arrays_IO_c.h"

struct MHD_boundary_ctl_c{
	int maxlen;
	
	struct chara2_real_ctl_array *bc_T_ctl;
	struct chara2_real_ctl_array *bc_U_ctl;
	struct chara2_real_ctl_array *bc_P_ctl;
	struct chara2_real_ctl_array *bc_C_ctl;
	struct chara2_real_ctl_array *bc_B_ctl;
	struct chara2_real_ctl_array *bc_MP_ctl;
	struct chara2_real_ctl_array *bc_A_ctl;
	struct chara2_real_ctl_array *bc_J_ctl;
	struct chara2_real_ctl_array *bc_infty_ctl;
};

/* prototype */
void get_label_MHD_node_bc_ctl(int index, char *label);
void get_label_MHD_surf_bc_ctl(int index, char *label);

void alloc_MHD_node_bc_ctl_c(struct MHD_boundary_ctl_c *nod_bc_ctl);
void alloc_MHD_surf_bc_ctl_c(struct MHD_boundary_ctl_c *surf_bc_ctl);
void dealloc_MHD_boundary_ctl_c(struct MHD_boundary_ctl_c *bc_ctl);
int read_MHD_node_bc_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct MHD_boundary_ctl_c *nod_bc_ctl);
int read_MHD_surf_bc_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct MHD_boundary_ctl_c *surf_bc_ctl);
int write_MHD_node_bc_ctl_c(FILE *fp, int level, const char *label,
                            struct MHD_boundary_ctl_c *nod_bc_ctl);
int write_MHD_surf_bc_ctl_c(FILE *fp, int level, const char *label,
                            struct MHD_boundary_ctl_c *surf_bc_ctl);

#endif /* t_ctl_data_MHD_boundary_c_h_ */
