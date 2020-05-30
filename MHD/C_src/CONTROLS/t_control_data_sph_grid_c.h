/*
//  t_control_data_sph_grid_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/08.
*/

#ifndef t_control_data_sph_grid_c_h_
#define t_control_data_sph_grid_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_ctl_data_4_FEM_mesh_c.h"
#include "t_ctl_data_4_sphere_model_c.h"

#define NLBL_SPH_SHELL_CTL      3

struct parallel_sph_shell_control_c{
    int iflag_use_file;
	int maxlen;
	
	struct FEM_mesh_control_c *Fmesh_ctl;
	struct sphere_domain_ctl_c *sdctl_c;
	struct sphere_data_ctl_c *spctl_c;
};

/* prototypes */
void get_label_sph_shell_ctl(int index, char *label);

struct parallel_sph_shell_control_c * init_parallel_sph_shell_control_c();
void dealloc_parallel_sph_shell_control_c(struct parallel_sph_shell_control_c *shell_ctl);
void read_spherical_shell_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct parallel_sph_shell_control_c *shell_ctl);
int write_spherical_shell_ctl_c(FILE *fp, int level, const char *label, 
                                struct parallel_sph_shell_control_c *shell_ctl);

void read_spherical_shell_file_c(const char *file_name, char buf[LENGTHBUF],
			struct parallel_sph_shell_control_c *shell_ctl);
int write_spherical_shell_file_c(const char *file_name,
			struct parallel_sph_shell_control_c *shell_ctl);



#endif /* t_control_data_sph_grid_c_h_ */
