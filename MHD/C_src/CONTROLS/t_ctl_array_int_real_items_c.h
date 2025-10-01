 /*
//  t_ctl_array_int_real_items_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef T_CTL_ARRAY_INT_REAL_ITEMS_C_H_
#define T_CTL_ARRAY_INT_REAL_ITEMS_C_H_

#include <stdlib.h>

#include "skip_comment_c.h"
#include "t_ctl_array_single_items_c.h"
#include "t_control_int_real_IO.h"

/*  Prototypes */

extern void c_alloc_int_real_array(int num, void *f_ctl);
extern void c_dealloc_int_real_array(void *f_ctl);
extern void c_check_int_real_array(void *f_ctl);

extern void * c_int_real_array_block_name(void *f_ctl);
extern int    c_int_real_array_num(void *f_ctl);
extern int    c_int_real_array_i_tbl(int i_idx, void *f_ctl);
extern double c_int_real_array_r_tbl(int i_idx, void *f_ctl);
extern void   c_store_int_real_array(void *f_ctl, int idx_in, int i_in, double r_in);

extern void c_store_int_real_array(void *f_ctl, int idx_in, int i_in, double r_in);

struct int_real_ctl_item * init_f_ctl_ir_item(void *(*c_load_self)(void *f_parent),
											 void *f_parent);
void dealloc_f_ctl_ir_item(struct int_real_ctl_item *f_ir_item);

struct int_real_clist * init_f_ctl_ir_array(void *(*c_load_self)(void *f_parent), 
											void *f_parent);
void reflesh_f_ctl_ir_array(int num_array, struct int_real_clist *ir_clst);

#endif /* T_CTL_ARRAY_INT_REAL_ITEMS_C_H_ */
