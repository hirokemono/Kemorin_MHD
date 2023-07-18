 /*
//  t_ctl_array_chara2_items_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef T_CTL_ARRAY_CHARA2_ITEMS_C_H_
#define T_CTL_ARRAY_CHARA2_ITEMS_C_H_

#include <stdlib.h>

#include "skip_comment_c.h"
#include "t_ctl_array_single_items_c.h"
#include "t_control_chara2_IO.h"

/*  Prototypes */

extern void c_alloc_chara2_array(int num_array, void *f_ctl);
extern void c_dealloc_chara2_array(void *f_ctl);
extern void c_check_chara2_array(void *f_ctl);

extern void * c_chara2_array_block_name(void *f_ctl);
extern int    c_chara2_array_num(void *f_ctl);
extern void * c_chara2_array_c1_tbl(int idx, void *f_ctl);
extern void * c_chara2_array_c2_tbl(int idx, void *f_ctl);
extern void   c_store_chara2_array(void *f_ctl, int idx,
                                   char *c1_in, char *c2_in);
extern void   c_store_chara2_items(void *f_ctl, 
                                   char *c1_in, char *c2_in);


struct chara2_ctl_item * init_f_ctl_c2_item(void *(*c_load_self)(void *f_parent),
                                            void *f_parent);

struct chara2_clist * init_f_ctl_c2_array(void *(*c_load_self)(void *f_parent),
                                                void *f_parent);
void reflesh_f_ctl_c2_array(int num_array, struct chara2_clist *c2_clst);

#endif /* T_CTL_ARRAY_CHARA2_ITEMS_C_H_ */
