/*
//  control_3elements_IO_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#ifndef control_3elements_IO_c_h_
#define control_3elements_IO_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"

struct real3_ctl_item{
	
	int iflag;
	double r_data[3];
};

/* prototypes */


void init_real3_ctl_item_c(struct real3_ctl_item *r3_item);
int read_real3_ctl_item_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct real3_ctl_item *r3_item);
int write_real3_ctl_item_c(FILE *fp, int level, int maxlen, 
			const char *label, struct real3_ctl_item *r3_item);

#endif /* control_3elements_IO_c_h_ */
