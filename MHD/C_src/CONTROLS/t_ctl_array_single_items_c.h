 /*
//  t_ctl_array_single_items_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/15.
*/

#ifndef T_CTL_ARRAY_SINGLE_ITEMS_C_H_
#define T_CTL_ARRAY_SINGLE_ITEMS_C_H_

#include <stdlib.h>
#include "skip_comment_c.h"

struct f_ctl_chara_item{
	void * f_self;
	char * f_block_name;
	int * f_iflag;
	char * f_charavalue;
	
	char * c_block_name;
	
	char * c_charavalue;
};

struct f_ctl_chara_array{
	void * f_self;
	char * f_block_name;
	int * f_num;
	int * f_icou;
	char * f_cctls;
	
	char * c_block_name;
	char ** c_charavalue;
};

struct f_ctl_real_item{
	void * f_self;
	char * f_block_name;
	int * f_iflag;
	double * f_realvalue;
	
	char * c_block_name;
};

struct f_ctl_real_array{
	void * f_self;
	char * f_block_name;
	int * f_num;
	int * f_icou;
	double * f_rctls;
	
	char * c_block_name;
};

struct f_ctl_int_item{
	void * f_self;
	char * f_block_name;
	int * f_iflag;
	int * f_intvalue;
	
	char * c_block_name;
};

struct f_ctl_int_array{
	void * f_self;
	char * f_block_name;
	int * f_num;
	int * f_icou;
	int * f_ictls;
	
	char * c_block_name;
};

/*  Prototypes */

extern int lengthchara_f();
extern char * strngcopy_from_f(char * f_char);
extern void load_chara_from_c(char *c_ctl);


struct f_ctl_chara_item * init_f_ctl_chara_item(void *(*c_load_self)(void *f_parent),
												void *f_parent);
void dealloc_f_ctl_chara_item(struct f_ctl_chara_item *f_citem);
struct f_ctl_chara_array * init_f_ctl_chara_array(void *(*c_load_self)(void *f_parent),
												  void *f_parent);
void reflesh_f_ctl_chara_array(int num_array, struct f_ctl_chara_array *f_carray);
void dealloc_f_ctl_chara_array(struct f_ctl_chara_array *f_carray);

struct c_array_views * init_c_array_views(struct f_ctl_chara_array *f_carray);
void dealloc_c_array_views(struct c_array_views *c_array_vws);

struct f_ctl_int_item * init_f_ctl_int_item(void *(*c_load_self)(void *f_parent), 
	void *f_parent);
void dealloc_f_ctl_int_item(struct f_ctl_int_item *f_ritem);
struct f_ctl_int_array * init_f_ctl_int_array(void *(*c_load_self)(void *f_parent), 
	void *f_parent);
void reflesh_f_ctl_int_array(int num_array, struct f_ctl_int_array *f_iarray);
void dealloc_f_ctl_int_array(struct f_ctl_int_array *f_iarray);


struct f_ctl_real_item * init_f_ctl_real_item(void *(*c_load_self)(void *f_parent), 
											  void *f_parent);
void dealloc_f_ctl_real_item(struct f_ctl_real_item *f_ritem);
struct f_ctl_real_array * init_f_ctl_real_array(void *(*c_load_self)(void *f_parent), 
												void *f_parent);
void reflesh_f_ctl_real_array(int num_array, struct f_ctl_real_array *f_rarray);
void dealloc_f_ctl_real_array(struct f_ctl_real_array *f_rarray);


#endif /* T_CTL_ARRAY_SINGLE_ITEMS_C_H_ */
