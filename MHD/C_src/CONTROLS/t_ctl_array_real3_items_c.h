 /*
//  t_ctl_array_real3_items_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef T_CTL_ARRAY_REAL3_ITEMS_C_H_
#define T_CTL_ARRAY_REAL3_ITEMS_C_H_

#include <stdlib.h>

#include "skip_comment_c.h"
#include "t_ctl_array_single_items_c.h"
#include "t_control_real3_IO.h"

/*  Prototypes */

extern void *c_real3_array_block_name(void *f_ctl);
extern int c_real3_array_num(void *f_ctl);
extern double c_real3_array_r1_tbl(void *f_ctl, int idx_in);
extern double c_real3_array_r2_tbl(void *f_ctl, int idx_in);
extern double c_real3_array_r3_tbl(void *f_ctl, int idx_in);
extern void c_store_real3_array(void *f_ctl, int idx_in,
								double r1_in, double r2_in, double r3_in);
extern void c_store_real3_items(void *f_ctl,
								double r1_in, double r2_in, double r3_in);


struct real3_ctl_item * init_f_ctl_r3_item(void *(*c_load_self)(void *f_parent),
											 void *f_parent);

struct real3_clist * init_f_ctl_r3_array(void *(*c_load_self)(void *f_parent), 
										 void *f_parent);
void reflesh_f_ctl_r3_array(int num_array, struct real3_clist *r3_clist);

#endif /* T_CTL_ARRAY_REAL3_ITEMS_C_H_ */
