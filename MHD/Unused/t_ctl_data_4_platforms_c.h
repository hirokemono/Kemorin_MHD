/*
//  t_ctl_data_4_platforms_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/05/18.
*/

#ifndef t_ctl_data_4_platforms_c_h__
#define t_ctl_data_4_platforms_c_h__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_int_IO.h"
#include "t_control_chara_IO.h"

#define NLBL_PLATFORM_CTL 20

struct platform_data_control_c{
    int maxlen;
    
    int iflag_use;
    
    struct int_ctl_item *ndomain_c;
    struct int_ctl_item *num_smp_c;

    struct chara_ctl_item *debug_flag_c;

    struct chara_ctl_item *sph_file_prefix_c;
    struct chara_ctl_item *mesh_file_prefix_c;
    struct chara_ctl_item *field_file_prefix_c;
    struct chara_ctl_item *restart_file_prefix_c;
    struct chara_ctl_item *spectr_field_file_prefix_c;

    struct chara_ctl_item *coriolis_int_file_name_c;
    struct chara_ctl_item *bc_data_file_name_c;
    struct chara_ctl_item *interpolate_sph_to_fem_c;
    struct chara_ctl_item *interpolate_fem_to_sph_c;
    
    struct chara_ctl_item *sph_file_fmt_c;
    struct chara_ctl_item *mesh_file_fmt_c;
    struct chara_ctl_item *restart_file_fmt_c;
    struct chara_ctl_item *field_file_fmt_c;
    struct chara_ctl_item *itp_file_fmt_c;
    struct chara_ctl_item *spectr_field_fmt_c;
    struct chara_ctl_item *coriolis_file_fmt_c;
    
    struct chara_ctl_item *del_org_data_ctl_c;
};

    
/* prototypes */
void get_label_platform_ctl(int index, char *label);

struct platform_data_control_c * init_platform_data_control_c();
void dealloc_platform_data_control_c(struct platform_data_control_c *files);

void read_platform_data_control_c(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct platform_data_control_c *files);
int write_platform_data_control_c(FILE *fp, int level, const char *label, 
                                  struct platform_data_control_c *files);


#endif /* t_ctl_data_4_platforms_c_h */
