/*
//  t_ctl_data_4_time_steps_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/05/18.
*/

#ifndef t_ctl_data_4_time_steps_c_h__
#define t_ctl_data_4_time_steps_c_h__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"

struct time_data_control_c{
    int iflag_use;
    int maxlen;
    
    struct int_ctl_item *i_step_init_c;
    struct int_ctl_item *i_step_number_c;
    struct real_ctl_item *elapsed_time_c;
    
    struct int_ctl_item *i_step_check_c;
    struct int_ctl_item *i_step_rst_c;
    struct int_ctl_item *i_step_pvr_c;
    struct int_ctl_item *i_step_psf_c;
    struct int_ctl_item *i_step_iso_c;
    struct int_ctl_item *i_step_ucd_c;
    struct int_ctl_item *i_step_fline_c;
    struct int_ctl_item *i_step_lic_c;
    
    struct int_ctl_item *i_step_monitor_c;
    struct int_ctl_item *i_step_sgs_coefs_c;
    struct int_ctl_item *i_step_boundary_c;
    
    struct real_ctl_item *dt_c;
    struct real_ctl_item *time_init_c;
    struct int_ctl_item *i_diff_steps_c;
    
    struct chara_ctl_item *flexible_step_c;
    
    struct int_ctl_item *start_rst_step_c;
    struct int_ctl_item *end_rst_step_c;
    struct real_ctl_item *min_delta_t_c;
    struct real_ctl_item *max_delta_t_c;
    struct real_ctl_item *min_eps_to_expand_c;
    struct real_ctl_item *max_eps_to_shrink_c;
    
    struct real_ctl_item *delta_t_check_c;
    struct real_ctl_item *delta_t_rst_c;
    struct real_ctl_item *delta_t_field_c;
    struct real_ctl_item *delta_t_psf_c;
    struct real_ctl_item *delta_t_iso_c;
    struct real_ctl_item *delta_t_pvr_c;
    struct real_ctl_item *delta_t_fline_c;
    struct real_ctl_item *delta_t_lic_c;
    struct real_ctl_item *delta_t_monitor_c;
    struct real_ctl_item *delta_t_sgs_coefs_c;
    struct real_ctl_item *delta_t_boundary_c;
};

/* prototype */
void get_label_time_data_ctl(int index, char *label);

struct time_data_control_c * init_time_data_control_c();
void dealloc_time_data_control_c(struct time_data_control_c *tctl);

void read_time_data_control_c(FILE *fp, char buf[LENGTHBUF], const char *label,
			struct time_data_control_c *tctl);
int write_time_data_control_c(FILE *fp, int level, const char *label, 
			struct time_data_control_c *tctl);



#endif /* t_ctl_data_4_time_steps_c_h */
