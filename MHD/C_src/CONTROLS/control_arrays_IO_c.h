/*
//  control_arrays_IO_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/05/24.
*/

#ifndef control_arrays_IO_c_h__
#define control_arrays_IO_c_h__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "kemosrc_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"

struct int_ctl_array{
    int maxlen;
    
    int num;
    int icou;
    struct int_ctl_item **i_array_item;
};

/* prototype */


void init_ctl_int_array(struct int_ctl_array *i_array);
void alloc_ctl_int_array(struct int_ctl_array *i_array);
void dealloc_ctl_int_array(struct int_ctl_array *i_array);
void read_integer_ctl_array_c(FILE *fp, char *buf, const char *label,
			struct int_ctl_array *i_array);
void write_integer_ctl_array_c(FILE *fp, int level, int maxlen,
			const char *label, struct int_ctl_array *i_array);

#endif /* control_arrays_IO_c.h */
