/*
//  t_ctl_array_int2_items_c.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

extern void * c_chara_item_clength(void *f_plt, int *length);

extern void c_alloc_int2_array(void *f_ctl);
extern void c_dealloc_int2_array(void *f_ctl);
extern void c_check_int2_array(void *f_ctl);

extern void * c_int2_item_block_name(void *f_ctl);
extern void * c_int2_item_iflag(void *f_ctl);
extern void * c_int2_item_intvalue(void *f_ctl);

extern void * c_int2_array_block_name(void *f_ctl);
extern void * c_int2_array_num(void *f_ctl);
extern void * c_int2_array_icou(void *f_ctl);
extern void * c_int2_array_i1_tbl(void *f_ctl);
extern void * c_int2_array_i2_tbl(void *f_ctl);

#include "t_ctl_array_int2_items_c.h"

struct f_ctl_i2_item * init_f_ctl_i2_item(void *(*c_load_self)(void *f_parent),
										  void *f_parent)
{
	struct f_ctl_i2_item *f_i2_item = (struct f_ctl_i2_item *) malloc(sizeof(struct f_ctl_i2_item));
	if(f_i2_item == NULL){
		printf("malloc error for f_ctl_i2_item\n");
		exit(0);
	};
	f_i2_item->f_self =  c_load_self(f_parent);
	
	f_i2_item->f_iflag =        (int *) c_int2_item_iflag(f_i2_item->f_self);
	f_i2_item->f_block_name =  (char *) c_int2_item_block_name(f_i2_item->f_self);
	f_i2_item->c_block_name = strngcopy_from_f(f_i2_item->f_block_name);
	
	f_i2_item->f_intvalue =   (int *)    c_int2_item_intvalue(f_i2_item->f_self);
	
	return f_i2_item;
}

void dealloc_f_ctl_i2_item(struct f_ctl_i2_item *f_i2_item)
{
	free(f_i2_item->c_block_name);
	
	f_i2_item->f_intvalue = NULL;
	f_i2_item->f_iflag = NULL;
	f_i2_item->f_block_name = NULL;
	f_i2_item->f_self = NULL;
	return;
}


struct f_ctl_i2_array * init_f_ctl_i2_array(void *(*c_load_self)(void *f_parent), 
											void *f_parent)
{
	struct f_ctl_i2_array *f_i2_array = (struct f_ctl_i2_array *) malloc(sizeof(struct f_ctl_i2_array));
	if(f_i2_array == NULL){
		printf("malloc error for f_ctl_i2_array\n");
		exit(0);
	};
	f_i2_array->f_self =  c_load_self(f_parent);
	
	f_i2_array->f_num =         (int *)  c_int2_array_num(f_i2_array->f_self);
	f_i2_array->f_icou =        (int *)  c_int2_array_icou(f_i2_array->f_self);
	f_i2_array->f_block_name =  (char *) c_int2_array_block_name(f_i2_array->f_self);
	f_i2_array->c_block_name = strngcopy_from_f(f_i2_array->f_block_name);
	
	f_i2_array->f_i1ctls =       (int *) c_int2_array_i1_tbl(f_i2_array->f_self);
	f_i2_array->f_i2ctls =       (int *) c_int2_array_i2_tbl(f_i2_array->f_self);
	return f_i2_array;
}

void reflesh_f_ctl_i2_array(int num_array, struct f_ctl_i2_array *f_i2_array)
{
/*	c_check_int2_array(f_i2_array->f_self); */

	c_dealloc_int2_array(f_i2_array->f_self);
	f_i2_array->f_num[0] = num_array;
	f_i2_array->f_icou[0] = num_array;
	c_alloc_int2_array(f_i2_array->f_self);
	
	f_i2_array->f_i1ctls =       (int *) c_int2_array_i1_tbl(f_i2_array->f_self);
	f_i2_array->f_i2ctls =       (int *) c_int2_array_i2_tbl(f_i2_array->f_self);
	return;
}

void dealloc_f_ctl_i2_array(struct f_ctl_i2_array *f_i2_array)
{
	int i;
	free(f_i2_array->c_block_name);
	
	f_i2_array->f_i1ctls = NULL;
	f_i2_array->f_i2ctls = NULL;
	f_i2_array->f_icou = NULL;
	f_i2_array->f_num = NULL;
	f_i2_array->f_block_name = NULL;
	f_i2_array->f_self = NULL;
	return;
}
