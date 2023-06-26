 /*
//  t_ctl_array_int2_items_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef T_CTL_ARRAY_INT2_ITEMS_C_H_
#define T_CTL_ARRAY_INT2_ITEMS_C_H_

#include <stdlib.h>

#include "skip_comment_c.h"
#include "t_ctl_array_single_items_c.h"

struct f_ctl_i2_item{
	void * f_self;
	char * f_block_name;
	int * f_iflag;
	int *f_intvalue;
	
	char * c_block_name;
};

struct f_ctl_i2_array{
	void * f_self;
	char * f_block_name;
	int * f_num;
	int * f_icou;
	int *f_i1ctls;
	int *f_i2ctls;
	
	char * c_block_name;
};


/*  Prototypes */

struct f_ctl_i2_item * init_f_ctl_i2_item(void *(*c_load_self)(void *f_parent), 
											 void *f_parent);
void dealloc_f_ctl_i2_item(struct f_ctl_i2_item *f_i2_item);

struct f_ctl_i2_array * init_f_ctl_i2_array(void *(*c_load_self)(void *f_parent), 
											void *f_parent);
void reflesh_f_ctl_i2_array(int num_array, struct f_ctl_i2_array *f_i2_array);
void dealloc_f_ctl_i2_array(struct f_ctl_i2_array *f_i2_array);

#endif /* T_CTL_ARRAY_INT2_ITEMS_C_H_ */
