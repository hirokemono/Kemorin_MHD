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
#include "t_control_real_IO.h"
#include "t_control_int_IO.h"
#include "t_control_chara_IO.h"

struct f_ctl_chara_item{
	void * f_self;
	int * f_iflag;
	char * f_charavalue;
	
	char * c_block_name;
	
	char * c_charavalue;
};

struct f_ctl_real_item{
	void * f_self;
	int * f_iflag;
	double * f_realvalue;
	
	char * c_block_name;
};

struct f_ctl_int_item{
	void * f_self;
	int * f_iflag;
	int * f_intvalue;
	
	char * c_block_name;
};

struct f_ctl_int_array{
	void * f_self;
	int * f_num;
	int * f_icou;
	int * f_ictls;
	
	char * c_block_name;
};

/*  Prototypes */

extern int lengthchara_f();
extern char * strngcopy_from_f(char * f_char);
extern void load_chara_from_c(char *c_ctl);

extern void * c_chara_array_block_name(void *f_ctl);
extern int    c_chara_array_num(void *f_ctl);
extern void * c_chara_array_c_tbl(int idx, void *f_ctl);
extern void   c_store_chara_array(void *f_ctl, int idx, char *c_in);

extern void * c_dealloc_chara_array(void *f_ctl);
extern void * c_alloc_chara_array(int num_array, void *f_ctl);
extern void * c_check_chara_array(void *f_ctl);

extern void * c_real_array_block_name(void *f_ctl);
extern int    c_real_array_num(void *f_ctl);
extern double c_real_array_r_tbl(int idx, void *f_ctl);
extern void c_store_real_array(void *f_ctl, int idx, double r_in);

extern void c_dealloc_real_array(void *f_ctl);
extern void c_alloc_real_array(int num_array, void *f_ctl);
extern void c_check_real_array(void *f_ctl);

extern void * c_int_array_block_name(void *f_ctl);
extern void * c_int_array_num(void *f_ctl);
extern void * c_int_array_icou(void *f_ctl);
extern void * c_int_array_i_tbl(void *f_ctl);

extern void * c_dealloc_int_array(void *f_ctl);
extern void * c_alloc_int_array(void *f_ctl);
extern void * c_check_int_array(void *f_ctl);


struct f_ctl_chara_item * init_f_ctl_chara_item(void *(*c_load_self)(void *f_parent),
												void *f_parent);
void dealloc_f_ctl_chara_item(struct f_ctl_chara_item *f_citem);
struct chara_clist * init_f_ctl_chara_array(void *(*c_load_self)(void *f_parent),
											void *f_parent);
void reflesh_f_ctl_chara_array(int num_array, struct chara_clist *c_clist);

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
struct real_clist * init_f_ctl_real_array(void *(*c_load_self)(void *f_parent), 
												void *f_parent);
void reflesh_f_ctl_real_array(int num_array, struct real_clist *r_clist);


#endif /* T_CTL_ARRAY_SINGLE_ITEMS_C_H_ */
