/*
//  t_ctl_array_chara_int_items_c.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include "t_ctl_array_chara_int_items_c.h"

extern void * c_chara_item_clength(void *f_plt, int *length);

extern void * c_chara_int_item_block_name(void *f_ctl);
extern void * c_chara_int_item_iflag(void *f_ctl);
extern void * c_chara_int_item_charavalue(void *f_ctl);
extern void * c_chara_int_item_intvalue(void *f_ctl);

extern void * c_chara_int_array_block_name(void *f_ctl);
extern void * c_chara_int_array_num(void *f_ctl);
extern void * c_chara_int_array_icou(void *f_ctl);
extern void * c_chara_int_array_c_tbl(void *f_ctl);
extern void * c_chara_int_array_i_tbl(void *f_ctl);

struct f_ctl_ci_item * init_f_ctl_ci_item(void *(*c_load_self)(void *f_parent),
										  void *f_parent)
{
	struct f_ctl_ci_item *f_ci_item = (struct f_ctl_ci_item *) malloc(sizeof(struct f_ctl_ci_item));
	if(f_ci_item == NULL){
		printf("malloc error for f_ctl_ci_item\n");
		exit(0);
	};
	f_ci_item->f_self =  c_load_self(f_parent);
	
	f_ci_item->f_iflag =        (int *) c_chara_int_item_iflag(f_ci_item->f_self);
	f_ci_item->f_block_name =  (char *) c_chara_int_item_block_name(f_ci_item->f_self);
	f_ci_item->c_block_name = strngcopy_from_f(f_ci_item->f_block_name);
	
	f_ci_item->f_intvalue =  (int *) c_chara_int_item_intvalue(f_ci_item->f_self);
	f_ci_item->f_charavalue =  (char *) c_chara_int_item_charavalue(f_ci_item->f_self);
	f_ci_item->c_charavalue = strngcopy_from_f(f_ci_item->f_charavalue);
	
	/*
	printf("f_ci_item->f_self %p \n", f_ci_item->f_self);
	printf("f_ci_item->c_block_name %s \n", f_ci_item->c_block_name);
	printf("f_ci_item->c_charavalue %d %s \n", 
		   f_ci_item->f_iflag[0], f_ci_item->c_charavalue, f_ci_item->c_charavalue);
	*/
	return f_ci_item;
}

void dealloc_f_ctl_ci_item(struct f_ctl_ci_item *f_ci_item)
{
	free(f_ci_item->c_charavalue);
	free(f_ci_item->c_block_name);
	
	f_ci_item->f_intvalue = NULL;
	f_ci_item->f_charavalue = NULL;
	f_ci_item->f_iflag = NULL;
	f_ci_item->f_block_name = NULL;
	f_ci_item->f_self = NULL;
	return;
}


struct f_ctl_ci_array * init_f_ctl_ci_array(void *(*c_load_self)(void *f_parent), 
											void *f_parent)
{
	struct f_ctl_ci_array *f_ci_array = (struct f_ctl_ci_array *) malloc(sizeof(struct f_ctl_ci_array));
	if(f_ci_array == NULL){
		printf("malloc error for f_ctl_ci_array\n");
		exit(0);
	};
	f_ci_array->f_self =  c_load_self(f_parent);
	
	f_ci_array->f_num =         (int *)  c_chara_int_array_num(f_ci_array->f_self);
	f_ci_array->f_icou =        (int *)  c_chara_int_array_icou(f_ci_array->f_self);
	f_ci_array->f_block_name =  (char *) c_chara_int_array_block_name(f_ci_array->f_self);
	f_ci_array->c_block_name = strngcopy_from_f(f_ci_array->f_block_name);
	
	f_ci_array->f_ictls =       (int *) c_chara_int_array_i_tbl(f_ci_array->f_self);
	f_ci_array->f_cctls =       (char *) c_chara_int_array_c_tbl(f_ci_array->f_self);
	
	f_ci_array->c_charavalue = (char **) malloc(f_ci_array->f_num[0] * sizeof(char *));
	if(f_ci_array->c_charavalue == NULL){
		printf("malloc error for f_ci_array->c_charavalue \n");
		exit(0);
	};
	
	int i;
	int flen = 255;
	for(i=0;i<f_ci_array->f_num[0];i++){
		f_ci_array->c_charavalue[i] = strngcopy_from_f(&f_ci_array->f_cctls[i*flen]);
	};
	
	printf("f_ci_array->f_self %p \n", f_ci_array->f_self);
	printf("f_ci_array->c_block_name %s %d\n", f_ci_array->c_block_name, f_ci_array->f_num[0]);
	for(i=0;i<f_ci_array->f_num[0];i++){
		printf("%d f_ci_array->c_charavalue %s %d\n", i,
			   f_ci_array->c_charavalue[i], f_ci_array->f_ictls[i]);
	}
	
	return f_ci_array;
}

void reflesh_f_ctl_ci_array(int num_array, struct f_ctl_ci_array *f_ci_array)
{
	c_dealloc_chara_int_array(f_ci_array->f_self);
	f_ci_array->f_num[0] = num_array;
	f_ci_array->f_icou[0] = num_array;
	c_alloc_chara_int_array(f_ci_array->f_self);
	
	f_ci_array->f_cctls =       (char *) c_chara_int_array_c_tbl(f_ci_array->f_self);
	f_ci_array->f_ictls =       (int *) c_chara_int_array_i_tbl(f_ci_array->f_self);
	return;
}

void dealloc_f_ctl_ci_array(struct f_ctl_ci_array *f_ci_array)
{
	int i;
	for(i=0;i<f_ci_array->f_num[0];i++){free(f_ci_array->c_charavalue[i]);};
	free(f_ci_array->c_charavalue);
	free(f_ci_array->c_block_name);
	
	f_ci_array->f_ictls = NULL;
	f_ci_array->f_cctls = NULL;
	f_ci_array->f_icou = NULL;
	f_ci_array->f_num = NULL;
	f_ci_array->f_block_name = NULL;
	f_ci_array->f_self = NULL;
	return;
}
