/*
//  t_ctl_data_crust_filter_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/05/18.
*/

#ifndef t_ctl_data_crust_filter_c__
#define t_ctl_data_crust_filter_c__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "control_elements_IO_c.h"
#include "t_control_int_IO.h"

#define NLBL_CRUSTAL_FILTER_CTL 1

struct crustal_filter_ctl_c{
    int maxlen;
    
    int iflag_use;
    
    struct int_ctl_item *crustal_truncation_c;
};

    
/* prototypes */
void get_label_crustal_filter_ctl(int index, char *label);

struct crustal_filter_ctl_c * init_crustal_filter_ctl_c();
void dealloc_crustal_filter_ctl_c(struct crustal_filter_ctl_c *crust_f_ctl);

void read_crustal_filter_ctl_c(FILE *fp, char buf[LENGTHBUF], const char *label, 
			struct crustal_filter_ctl_c *crust_f_ctl);
int write_crustal_filter_ctl_c(FILE *fp, int level, const char *label, 
                                  struct crustal_filter_ctl_c *crust_f_ctl);


#endif /* t_ctl_data_crust_filter_c__ */
