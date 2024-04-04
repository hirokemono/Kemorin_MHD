 /*
//  t_ctl_array_chara3_items_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef T_CTL_ARRAY_CHARA3_ITEMS_C_H_
#define T_CTL_ARRAY_CHARA3_ITEMS_C_H_

#include <stdlib.h>

#include "skip_comment_c.h"
#include "t_ctl_array_single_items_c.h"
#include "t_control_chara3_IO.h"

/*  Prototypes */

extern void c_alloc_chara3_array(int num, void *f_ctl);
extern void c_dealloc_chara3_array(void *f_ctl);
extern void c_check_chara3_array(void *f_ctl);

extern void *c_chara3_array_block_name(void *f_ctl);
extern int   c_chara3_array_num(void *f_ctl);
extern void *c_chara3_array_c1_tbl(int idx_in, void *f_ctl);
extern void *c_chara3_array_c2_tbl(int idx_in, void *f_ctl);
extern void *c_chara3_array_c3_tbl(int idx_in, void *f_ctl);
extern void  c_store_chara3_array(void *f_ctl, int idx_in,
								  char *c1_in, char *c2_in, char *c3_in);
extern void  c_store_chara3_items(void *f_ctl, char *c1_in, char *c2_in, char *c3_in);


struct chara3_ctl_item * init_f_ctl_c3_item(void *(*c_load_self)(void *f_parent),
											 void *f_parent);

struct chara3_clist * init_f_ctl_c3_array(void *(*c_load_self)(void *f_parent), 
										  void *f_parent);
void reflesh_f_ctl_c3_array(int num_array, struct chara3_clist *c3_clist);

#endif /* T_CTL_ARRAY_CHARA3_ITEMS_C_H_ */
