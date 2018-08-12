/*
//  t_ctl_data_node_monitor_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/08.
*/

#ifndef t_ctl_data_node_monitor_c_h_
#define t_ctl_data_node_monitor_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "control_elements_IO_c.h"
#include "control_arrays_IO_c.h"
#include "t_control_chara_IO.h"
#include "t_control_int2_IO.h"
#include "t_control_real3_IO.h"

struct node_monitor_ctl_c{
    int maxlen;
    
    struct chara_ctl_list group_4_monitor_list;
    struct real3_ctl_list xx_4_monitor_list;
    struct int2_ctl_list node_4_monitor_list;
};

/* prototype */
void get_label_node_monitor_ctl(int index, char *label);

void alloc_node_monitor_ctl_c(struct node_monitor_ctl_c *nmtr_ctl);
void dealloc_node_monitor_ctl_c(struct node_monitor_ctl_c *nmtr_ctl);
int read_node_monitor_ctl_c(FILE *fp, char buf[LENGTHBUF],
			const char *label, struct node_monitor_ctl_c *nmtr_ctl);
int write_node_monitor_ctl_c(FILE *fp, int level, const char *label, 
			struct node_monitor_ctl_c *nmtr_ctl);


#endif /* t_ctl_data_node_monitor_c_h_ */
