/*
//  t_control_data_gen_sph_grid_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/08.
*/

#ifndef t_control_data_gen_sph_grid_c_h_
#define t_control_data_gen_sph_grid_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "t_ctl_data_4_platforms_c.h"
#include "t_control_data_sph_grid_c.h"

struct gen_sph_grid_ctl_c{
    int maxlen;
	
	int iflag_platform_data_ctl;
    struct platform_data_control_c *files;
	int iflag_shell_ctl;
	char *fname_shell_ctl;
    struct parallel_sph_shell_control_c *shell_ctl;
};

/* prototypes */

void get_label_gen_sph_grid_ctl(int index, char *label);

void alloc_gen_sph_shell_ctl_c(struct gen_sph_grid_ctl_c *gen_sph_c);
void dealloc_gen_sph_shell_ctl_c(struct gen_sph_grid_ctl_c *gen_sph_c);
int read_gen_sph_shell_ctl_c(FILE *fp, char buf[LENGTHBUF], 
                           const char *label, struct gen_sph_grid_ctl_c *gen_sph_c);
int write_gen_sph_shell_ctl_c(FILE *fp, int level, const char *label, 
                            struct gen_sph_grid_ctl_c *gen_sph_c);

void rename_gen_sph_subfiles_c(struct gen_sph_grid_ctl_c *gen_sph_c);
int read_gen_sph_shell_file_c(const char *file_name, char buf[LENGTHBUF],
                        struct gen_sph_grid_ctl_c *gen_sph_c);
int write_gen_sph_shell_file_c(const char *file_name, struct gen_sph_grid_ctl_c *gen_sph_c);


#endif /* t_control_data_gen_sph_grid_c_h_ */
