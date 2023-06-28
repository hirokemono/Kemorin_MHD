 /*
//  t_ctl_array_chara_real_items_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef T_CTL_ARRAY_CHARA_REAL_ITEMS_C_H_
#define T_CTL_ARRAY_CHARA_REAL_ITEMS_C_H_

#include <stdlib.h>

#include "skip_comment_c.h"
#include "t_ctl_array_single_items_c.h"
#include "t_control_chara_real_IO.h"

struct f_ctl_cr_item{
	void * f_self;
	int * f_iflag;
	char * f_charavalue;
	double * f_realvalue;
	
	char * c_block_name;
	
	char * c_charavalue;
};

/*  Prototypes */

extern void * c_chara_real_array_block_name(void *f_ctl);
extern int    c_chara_real_array_num(void *f_ctl);
extern void * c_chara_real_array_c_tbl(int idx, void *f_ctl);
extern double c_chara_real_array_r_tbl(int idx, void *f_ctl);
extern void   c_store_chara_real_array(void *f_ctl, int idx, char *c_in, double r_in);

extern void c_alloc_chara_real_array(int num_array, void *f_ctl);
extern void c_dealloc_chara_real_array(void *f_ctl);
extern void c_check_chara_real_array(void *f_ctl);

struct f_ctl_cr_item * init_f_ctl_cr_item(void *(*c_load_self)(void *f_parent), 
											 void *f_parent);
void dealloc_f_ctl_cr_item(struct f_ctl_cr_item *f_cr_item);

struct chara_real_clist * init_f_ctl_cr_array(void *(*c_load_self)(void *f_parent), 
											void *f_parent);
void reflesh_f_ctl_cr_array(int num_array, struct chara_real_clist *cr_clst);

#endif /* T_CTL_ARRAY_CHARA_REAL_ITEMS_C_H_ */
