/*
//  t_ctl_array_int_real_items_c.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

extern void * c_chara_item_clength(void *f_plt, int *length);

extern void c_alloc_int_real_array(void *f_ctl);
extern void c_dealloc_int_real_array(void *f_ctl);
extern void c_check_int_real_array(void *f_ctl);

extern void * c_int_real_item_block_name(void *f_ctl);
extern void * c_int_real_item_iflag(void *f_ctl);
extern void * c_int_real_item_intvalue(void *f_ctl);
extern void * c_int_real_item_realvalue(void *f_ctl);

extern void * c_int_real_array_block_name(void *f_ctl);
extern void * c_int_real_array_num(void *f_ctl);
extern void * c_int_real_array_icou(void *f_ctl);
extern void * c_int_real_array_i_tbl(void *f_ctl);
extern void * c_int_real_array_r_tbl(void *f_ctl);

#include "t_ctl_array_int_real_items_c.h"

struct f_ctl_ir_item * init_f_ctl_ir_item(void *(*c_load_self)(void *f_parent),
										  void *f_parent)
{
	struct f_ctl_ir_item *f_ir_item = (struct f_ctl_ir_item *) malloc(sizeof(struct f_ctl_ir_item));
	if(f_ir_item == NULL){
		printf("malloc error for f_ctl_ir_item\n");
		exit(0);
	};
	f_ir_item->f_self =  c_load_self(f_parent);
	
	f_ir_item->f_iflag =        (int *) c_int_real_item_iflag(f_ir_item->f_self);
	f_ir_item->f_block_name =  (char *) c_int_real_item_block_name(f_ir_item->f_self);
	f_ir_item->c_block_name = strngcopy_from_f(f_ir_item->f_block_name);
	
	f_ir_item->f_intvalue =   (int *)    c_int_real_item_intvalue(f_ir_item->f_self);
	f_ir_item->f_realvalue =  (double *) c_int_real_item_realvalue(f_ir_item->f_self);
	
	return f_ir_item;
}

void dealloc_f_ctl_ir_item(struct f_ctl_ir_item *f_ir_item)
{
	free(f_ir_item->c_block_name);
	
	f_ir_item->f_realvalue = NULL;
	f_ir_item->f_intvalue = NULL;
	f_ir_item->f_iflag = NULL;
	f_ir_item->f_block_name = NULL;
	f_ir_item->f_self = NULL;
	return;
}


struct f_ctl_ir_array * init_f_ctl_ir_array(void *(*c_load_self)(void *f_parent), 
											void *f_parent)
{
	struct f_ctl_ir_array *f_ir_array = (struct f_ctl_ir_array *) malloc(sizeof(struct f_ctl_ir_array));
	if(f_ir_array == NULL){
		printf("malloc error for f_ctl_ir_array\n");
		exit(0);
	};
	f_ir_array->f_self =  c_load_self(f_parent);
	
	f_ir_array->f_num =         (int *)  c_int_real_array_num(f_ir_array->f_self);
	f_ir_array->f_icou =        (int *)  c_int_real_array_icou(f_ir_array->f_self);
	f_ir_array->f_block_name =  (char *) c_int_real_array_block_name(f_ir_array->f_self);
	f_ir_array->c_block_name = strngcopy_from_f(f_ir_array->f_block_name);
	
	f_ir_array->f_ictls =       (int *)    c_int_real_array_i_tbl(f_ir_array->f_self);
	f_ir_array->f_rctls =       (double *) c_int_real_array_r_tbl(f_ir_array->f_self);
	return f_ir_array;
}

void reflesh_f_ctl_ir_array(int num_array, struct f_ctl_ir_array *f_ir_array)
{
/*	c_check_int_real_array(f_ir_array->f_self); */

	c_dealloc_int_real_array(f_ir_array->f_self);
	f_ir_array->f_num[0] = num_array;
	f_ir_array->f_icou[0] = num_array;
	c_alloc_int_real_array(f_ir_array->f_self);
	
	f_ir_array->f_ictls =       (int *)    c_int_real_array_i_tbl(f_ir_array->f_self);
	f_ir_array->f_rctls =       (double *) c_int_real_array_r_tbl(f_ir_array->f_self);
	return;
}

void dealloc_f_ctl_ir_array(struct f_ctl_ir_array *f_ir_array)
{
	int i;
	free(f_ir_array->c_block_name);
	
	f_ir_array->f_rctls = NULL;
	f_ir_array->f_ictls = NULL;
	f_ir_array->f_icou = NULL;
	f_ir_array->f_num = NULL;
	f_ir_array->f_block_name = NULL;
	f_ir_array->f_self = NULL;
	return;
}
