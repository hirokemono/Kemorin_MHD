 /*
//  t_ctl_array_chara_int_items_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef T_CTL_ARRAY_CHARA_INT_ITEMS_C_H_
#define T_CTL_ARRAY_CHARA_INT_ITEMS_C_H_

#include <stdlib.h>

#include "skip_comment_c.h"
#include "t_ctl_array_single_items_c.h"
#include "t_control_chara_int_IO.h"

/*  Prototypes */

extern void c_alloc_chara_int_array(int num, void *f_ctl);
extern void c_dealloc_chara_int_array(void *f_ctl);
extern void c_check_chara_int_array(void *f_ctl);

extern void *c_chara_int_array_block_name(void *f_ctl);
extern int c_chara_int_array_num(void *f_ctl);
extern void *c_chara_int_array_icou(void *f_ctl);
extern void *c_chara_int_array_c_tbl(int idx_in, void *f_ctl);
extern int   c_chara_int_array_i_tbl(int idx_in, void *f_ctl);
extern void  c_store_chara_int_array(void *f_ctl, int idx_in, char *c_in, int i_in);
extern void  c_store_chara_int_items(void *f_ctl, char *c_in, int i_in);


struct chara_int_ctl_item * init_f_ctl_ci_item(void *(*c_load_self)(void *f_parent),
                                                void *f_parent);

struct chara_int_clist * init_f_ctl_ci_array(void *(*c_load_self)(void *f_parent), 
											void *f_parent);
void reflesh_f_ctl_ci_array(int num_array, struct chara_int_clist *ci_clist);

#endif /* T_CTL_ARRAY_CHARA_INT_ITEMS_C_H_ */
