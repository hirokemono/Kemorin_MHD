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
#include "t_control_int2_IO.h"

struct f_ctl_i2_item{
	void * f_self;
	int * f_iflag;
	int *f_intvalue;
	
	char * c_block_name;
};

/*  Prototypes */

extern void *c_int2_array_block_name(void *f_ctl);
extern int c_int2_array_num(void *f_ctl);
extern int c_int2_array_i1_tbl(void *f_ctl, int idx_in);
extern int c_int2_array_i2_tbl(void *f_ctl, int idx_in);
extern void c_store_int2_array(void *f_ctl, int idx_in, int i1_in, int i2_in);

struct f_ctl_i2_item * init_f_ctl_i2_item(void *(*c_load_self)(void *f_parent), 
											 void *f_parent);
void dealloc_f_ctl_i2_item(struct f_ctl_i2_item *f_i2_item);

struct int2_clist * init_f_ctl_i2_array(void *(*c_load_self)(void *f_parent), 
											void *f_parent);
void reflesh_f_ctl_i2_array(int num_array, struct int2_clist *i2_clst);

#endif /* T_CTL_ARRAY_INT2_ITEMS_C_H_ */
