 /*
//  t_ctl_array_two_items_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef T_CTL_ARRAY_TWO_ITEMS_C_H_
#define T_CTL_ARRAY_TWO_ITEMS_C_H_

#include <stdlib.h>

#include "skip_comment_c.h"
#include "t_ctl_array_single_items_c.h"

struct f_ctl_ci_item{
	void * f_self;
	char * f_block_name;
	int * f_iflag;
	char * f_charavalue;
	int *  f_intvalue;
	
	char * c_block_name;
	char * c_charavalue;
};

struct f_ctl_ci_array{
	void * f_self;
	char * f_block_name;
	int * f_num;
	int * f_icou;
	char * f_cctls;
	int *  f_ictls;
	
	char * c_block_name;
	char ** c_charavalue;
};


/*  Prototypes */

struct f_ctl_ci_item * init_f_ctl_ci_item(void *(*c_load_self)(void *f_parent), 
											 void *f_parent);
void dealloc_f_ctl_ci_item(struct f_ctl_ci_item *f_ci_item);

struct f_ctl_ci_array * init_f_ctl_ci_array(void *(*c_load_self)(void *f_parent), 
											void *f_parent);
void reflesh_f_ctl_ci_array(int num_array, struct f_ctl_ci_array *f_ci_array);
void dealloc_f_ctl_ci_array(struct f_ctl_ci_array *f_ci_array);

#endif /* T_CTL_ARRAY_TWO_ITEMS_C_H_ */
