/*
//  t_ctl_array_chara_real_items_c.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include "t_ctl_array_chara_real_items_c.h"

extern void * c_chara_item_clength(void *f_plt, int *length);

extern void * c_chara_real_item_block_name(void *f_ctl);
extern void * c_chara_real_item_iflag(void *f_ctl);
extern void * c_chara_real_item_charavalue(void *f_ctl);
extern void * c_chara_real_item_realvalue(void *f_ctl);

struct f_ctl_cr_item * init_f_ctl_cr_item(void *(*c_load_self)(void *f_parent),
										  void *f_parent)
{
	struct f_ctl_cr_item *f_cr_item = (struct f_ctl_cr_item *) malloc(sizeof(struct f_ctl_cr_item));
	if(f_cr_item == NULL){
		printf("malloc error for f_ctl_cr_item\n");
		exit(0);
	};
	f_cr_item->f_self =  c_load_self(f_parent);
	
	f_cr_item->f_iflag =        (int *) c_chara_real_item_iflag(f_cr_item->f_self);
	f_cr_item->f_block_name =  (char *) c_chara_real_item_block_name(f_cr_item->f_self);
	f_cr_item->c_block_name = strngcopy_from_f(f_cr_item->f_block_name);
	
	f_cr_item->f_realvalue =  (double *) c_chara_real_item_realvalue(f_cr_item->f_self);
	f_cr_item->f_charavalue =  (char *) c_chara_real_item_charavalue(f_cr_item->f_self);
	f_cr_item->c_charavalue = strngcopy_from_f(f_cr_item->f_charavalue);
	
	/*
	printf("f_cr_item->f_self %p \n", f_cr_item->f_self);
	printf("f_cr_item->c_block_name %s \n", f_cr_item->c_block_name);
	printf("f_cr_item->c_charavalue %d %s \n", 
		   f_cr_item->f_iflag[0], f_cr_item->c_charavalue, f_cr_item->c_charavalue);
	*/
	return f_cr_item;
}

void dealloc_f_ctl_cr_item(struct f_ctl_cr_item *f_cr_item)
{
	free(f_cr_item->c_charavalue);
	free(f_cr_item->c_block_name);
	
	f_cr_item->f_realvalue = NULL;
	f_cr_item->f_charavalue = NULL;
	f_cr_item->f_iflag = NULL;
	f_cr_item->f_block_name = NULL;
	f_cr_item->f_self = NULL;
	return;
}


struct f_ctl_cr_array * init_f_ctl_cr_array(void *(*c_load_self)(void *f_parent), 
											void *f_parent)
{
	struct f_ctl_cr_array *f_cr_array = (struct f_ctl_cr_array *) malloc(sizeof(struct f_ctl_cr_array));
	if(f_cr_array == NULL){
		printf("malloc error for f_ctl_cr_array\n");
		exit(0);
	};
	f_cr_array->f_self =  c_load_self(f_parent);
	
	f_cr_array->f_num =         (int *)  c_chara_real_array_num(f_cr_array->f_self);
	f_cr_array->f_block_name =  (char *) c_chara_real_array_block_name(f_cr_array->f_self);
	f_cr_array->c_block_name = strngcopy_from_f(f_cr_array->f_block_name);
	
	f_cr_array->f_rctls =       (double *) c_chara_real_array_r_tbl(f_cr_array->f_self);
	f_cr_array->f_cctls =       (char *) c_chara_real_array_c_tbl(f_cr_array->f_self);
	
	f_cr_array->c_charavalue = (char **) malloc(f_cr_array->f_num[0] * sizeof(char *));
	if(f_cr_array->c_charavalue == NULL){
		printf("malloc error for f_cr_array->c_charavalue \n");
		exit(0);
	};
	
	int i;
	int flen = 255;
	for(i=0;i<f_cr_array->f_num[0];i++){
		f_cr_array->c_charavalue[i] = strngcopy_from_f(&f_cr_array->f_cctls[i*flen]);
	};
	
	printf("f_cr_array->f_self %p \n", f_cr_array->f_self);
	printf("f_cr_array->c_block_name %s %d\n", f_cr_array->c_block_name, f_cr_array->f_num[0]);
	for(i=0;i<f_cr_array->f_num[0];i++){
		printf("%d f_cr_array->c_charavalue %s %le\n", i,
			   f_cr_array->c_charavalue[i], f_cr_array->f_rctls[i]);
	}
	
	return f_cr_array;
}

void reflesh_f_ctl_cr_array(int num_array, struct f_ctl_cr_array *f_cr_array)
{
	c_dealloc_chara_real_array(f_cr_array->f_self);
	c_alloc_chara_real_array(num_array, f_cr_array->f_self);
	
	f_cr_array->f_cctls =       (char *) c_chara_real_array_c_tbl(f_cr_array->f_self);
	f_cr_array->f_rctls =       (double *) c_chara_real_array_r_tbl(f_cr_array->f_self);
	return;
}

void dealloc_f_ctl_cr_array(struct f_ctl_cr_array *f_cr_array)
{
	int i;
	for(i=0;i<f_cr_array->f_num[0];i++){free(f_cr_array->c_charavalue[i]);};
	free(f_cr_array->c_charavalue);
	free(f_cr_array->c_block_name);
	
	f_cr_array->f_rctls = NULL;
	f_cr_array->f_cctls = NULL;
	f_cr_array->f_icou = NULL;
	f_cr_array->f_num = NULL;
	f_cr_array->f_block_name = NULL;
	f_cr_array->f_self = NULL;
	return;
}
