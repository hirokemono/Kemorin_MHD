 /*
//  t_ctl_array_chara2_int_items_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef T_CTL_ARRAY_CHARA2_INT_ITEMS_C_H_
#define T_CTL_ARRAY_CHARA2_INT_ITEMS_C_H_

#include <stdlib.h>

#include "skip_comment_c.h"
#include "t_ctl_array_single_items_c.h"
#include "t_control_chara2_int_IO.h"

/*  Prototypes */

extern void c_alloc_chara2_int_array(int num_array, void *f_ctl);
extern void c_dealloc_chara2_int_array(void *f_ctl);
extern void c_check_chara2_int_array(void *f_ctl);

extern void * c_chara2_int_array_block_name(void *f_ctl);
extern int    c_chara2_int_array_num(void *f_ctl);
extern void * c_chara2_int_array_c1_tbl(int idx, void *f_ctl);
extern void * c_chara2_int_array_c2_tbl(int idx, void *f_ctl);
extern int    c_chara2_int_array_i_tbl(int idx, void *f_ctl);
extern void   c_store_chara2_int_array(void *f_ctl, int idx,
										char *c1_in, char *c2_in, int i_in);
extern void   c_store_chara2_int_items(void *f_ctl, 
										char *c1_in, char *c2_in, int i_in);


struct chara2_int_ctl_item * init_f_ctl_c2i_item(void *(*c_load_self)(void *f_parent),
                                                void *f_parent);

struct chara2_int_clist * init_f_ctl_c2i_array(void *(*c_load_self)(void *f_parent),
                                                void *f_parent);

struct chara2_int_clist * init_field_label_array(void *f_self);
int append_field_label_array(void *f_self, struct chara2_int_clist *c2i_clst);

void reflesh_f_ctl_c2i_array(int num_array, struct chara2_int_clist *c2i_clst);

#endif /* T_CTL_ARRAY_CHARA2_INT_ITEMS_C_H_ */
