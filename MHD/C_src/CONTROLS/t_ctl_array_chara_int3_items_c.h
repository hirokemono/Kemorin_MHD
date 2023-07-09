 /*
//  t_ctl_array_chara_int3_items_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef T_CTL_ARRAY_CHARA_INT3_ITEMS_C_H_
#define T_CTL_ARRAY_CHARA_INT3_ITEMS_C_H_

#include <stdlib.h>

#include "skip_comment_c.h"
#include "t_ctl_array_single_items_c.h"
#include "t_control_chara_int3_IO.h"

/*  Prototypes */

extern void c_alloc_chara_int3_array(int num, void *f_ctl);
extern void c_dealloc_chara_int3_array(void *f_ctl);
extern void c_check_chara_int3_array(void *f_ctl);

extern void *c_chara_int3_array_block_name(void *f_ctl);
extern int c_chara_int3_array_num(void *f_ctl);
extern void *c_chara_int3_array_icou(void *f_ctl);
extern void *c_chara_int3_array_c_tbl(int idx_in, void *f_ctl);
extern int   c_chara_int3_array_i1_tbl(int idx_in, void *f_ctl);
extern int   c_chara_int3_array_i2_tbl(int idx_in, void *f_ctl);
extern int   c_chara_int3_array_i3_tbl(int idx_in, void *f_ctl);
extern void  c_store_chara_int3_array(void *f_ctl, int idx_in, char *c_in,
									  int i1_in, int i2_in, int i3_in);
extern void  c_store_chara_int3_items(void *f_ctl, char *c_in,
									  int i1_in, int i2_in, int i3_in);


struct chara_int3_ctl_item * init_f_ctl_ci3_item(void *(*c_load_self)(void *f_parent),
											 void *f_parent);

struct chara_int3_clist * init_f_ctl_ci3_array(void *(*c_load_self)(void *f_parent),
											void *f_parent);
void reflesh_f_ctl_ci3_array(int num_array, struct chara_int3_clist *ci3_clist);

#endif /* T_CTL_ARRAY_CHARA_INT3_ITEMS_C_H_ */
