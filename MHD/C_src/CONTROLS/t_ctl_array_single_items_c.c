/*
//  t_ctl_array_single_items_c.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include "t_ctl_array_single_items_c.h"

extern void * c_chara_item_block_name(void *f_ctl);
extern void * c_chara_item_iflag(void *f_ctl);
extern void * c_chara_item_charavalue(void *f_ctl);

extern void * c_chara_array_block_name(void *f_ctl);
extern void * c_chara_array_num(void *f_ctl);
extern void * c_chara_array_icou(void *f_ctl);
extern void * c_chara_array_c_tbl(void *f_ctl);

extern void * c_dealloc_chara_array(void *f_ctl);
extern void * c_alloc_chara_array(void *f_ctl);
extern void * c_check_chara_array(void *f_ctl);


extern void * c_real_item_block_name(void *f_ctl);
extern void * c_real_item_iflag(void *f_ctl);
extern void * c_real_item_realvalue(void *f_ctl);

extern void * c_real_array_block_name(void *f_ctl);
extern void * c_real_array_num(void *f_ctl);
extern void * c_real_array_icou(void *f_ctl);
extern void * c_real_array_r_tbl(void *f_ctl);

extern void * c_dealloc_real_array(void *f_ctl);
extern void * c_alloc_real_array(void *f_ctl);
extern void * c_check_real_array(void *f_ctl);


extern void * c_int_item_block_name(void *f_ctl);
extern void * c_int_item_iflag(void *f_ctl);
extern void * c_int_item_intvalue(void *f_ctl);

extern void * c_int_array_block_name(void *f_ctl);
extern void * c_int_array_num(void *f_ctl);
extern void * c_int_array_icou(void *f_ctl);
extern void * c_int_array_i_tbl(void *f_ctl);

extern void * c_dealloc_int_array(void *f_ctl);
extern void * c_alloc_int_array(void *f_ctl);
extern void * c_check_int_array(void *f_ctl);

extern void * c_chara_item_clength(void *f_ctl, int *length);

char * strngcopy_from_f(char * f_char)
{
	char *c_char;
	int f_charlength[1];
	c_chara_item_clength(f_char, f_charlength);
	c_char = alloc_string((long) f_charlength[0]);
	strngcopy_w_length(c_char, f_charlength[0], f_char);
	return c_char;
}

struct f_ctl_chara_item * init_f_ctl_chara_item(void *(*c_load_self)(void *f_parent), 
												void *f_parent)
{
	struct f_ctl_chara_item *f_citem = (struct f_ctl_chara_item *) malloc(sizeof(struct f_ctl_chara_item));
	if(f_citem == NULL){
		printf("malloc error for f_ctl_chara_item\n");
		exit(0);
	};
	f_citem->f_self =  c_load_self(f_parent);
	
	f_citem->f_iflag =        (int *) c_chara_item_iflag(f_citem->f_self);
	f_citem->f_block_name =  (char *) c_chara_item_block_name(f_citem->f_self);
	f_citem->c_block_name = strngcopy_from_f(f_citem->f_block_name);
	
	f_citem->f_charavalue =  (char *) c_chara_item_charavalue(f_citem->f_self);
	f_citem->c_charavalue = strngcopy_from_f(f_citem->f_charavalue);
	
	return f_citem;
}


void dealloc_f_ctl_chara_item(struct f_ctl_chara_item *f_citem)
{
	free(f_citem->c_charavalue);
	free(f_citem->c_block_name);
	
	f_citem->f_charavalue = NULL;
	f_citem->f_iflag = NULL;
	f_citem->f_block_name = NULL;
	f_citem->f_self = NULL;
	return;
}

struct f_ctl_chara_array * init_f_ctl_chara_array(void *(*c_load_self)(void *f_parent), 
												  void *f_parent)
{
	struct f_ctl_chara_array *f_carray = (struct f_ctl_chara_array *) malloc(sizeof(struct f_ctl_chara_array));
	if(f_carray == NULL){
		printf("malloc error for f_ctl_chara_array\n");
		exit(0);
	};
	f_carray->f_self =  c_load_self(f_parent);
	
	f_carray->f_num =         (int *)  c_chara_array_num(f_carray->f_self);
	f_carray->f_icou =        (int *)  c_chara_array_icou(f_carray->f_self);
	f_carray->f_block_name =  (char *) c_chara_array_block_name(f_carray->f_self);
	f_carray->c_block_name = strngcopy_from_f(f_carray->f_block_name);
	
	f_carray->f_cctls =       (char *) c_chara_array_c_tbl(f_carray->f_self);
	
	f_carray->c_charavalue = (char **) malloc(f_carray->f_num[0] * sizeof(char *));
	if(f_carray->c_charavalue == NULL){
		printf("malloc error for f_carray->c_charavalue \n");
		exit(0);
	};
	
	int i;
	int flen = 255;
	for(i=0;i<f_carray->f_num[0];i++){
		f_carray->c_charavalue[i] = strngcopy_from_f(&f_carray->f_cctls[i*flen]);
	};
	/*
	printf("f_carray->f_self %p \n", f_carray->f_self);
	printf("f_carray->c_block_name %s %d\n", f_carray->c_block_name, f_carray->f_num[0]);
	for(i=0;i<f_carray->f_num[0];i++){
		printf("%d f_carray->c_charavalue %p %s \n", i, 
			   f_carray->c_charavalue[i], f_carray->c_charavalue[i]);
	}
	*/
	return f_carray;
}

void reflesh_f_ctl_chara_array(int num_array, struct f_ctl_chara_array *f_carray)
{
	c_dealloc_chara_array(f_carray->f_self);
	f_carray->f_num[0] = num_array;
	f_carray->f_icou[0] = num_array;
	c_alloc_chara_array(f_carray->f_self);
	
	f_carray->f_cctls =       (char *) c_chara_array_c_tbl(f_carray->f_self);
	
	f_carray->c_charavalue = (char **) malloc(f_carray->f_num[0] * sizeof(char *));
	if(f_carray->c_charavalue == NULL){
		printf("malloc error for f_carray->c_charavalue \n");
		exit(0);
	};
	
	int i;
	int flen = 255;
	for(i=0;i<f_carray->f_num[0];i++){
		f_carray->c_charavalue[i] = strngcopy_from_f(&f_carray->f_cctls[i*flen]);
	};
/*	c_check_chara_array(f_carray->f_self); */
	return;
}

void dealloc_f_ctl_chara_array(struct f_ctl_chara_array *f_carray)
{
	int i;
	for(i=0;i<f_carray->f_num[0];i++){free(f_carray->c_charavalue[i]);};
	free(f_carray->c_charavalue);
	free(f_carray->c_block_name);
	
	f_carray->f_cctls = NULL;
	f_carray->f_icou = NULL;
	f_carray->f_num = NULL;
	f_carray->f_block_name = NULL;
	f_carray->f_self = NULL;
	return;
}

struct f_ctl_real_item * init_f_ctl_real_item(void *(*c_load_self)(void *f_parent), 
												void *f_parent)
{
	struct f_ctl_real_item *f_ritem = (struct f_ctl_real_item *) malloc(sizeof(struct f_ctl_real_item));
	if(f_ritem == NULL){
		printf("malloc error for f_ctl_real_item\n");
		exit(0);
	};
	f_ritem->f_self =  c_load_self(f_parent);
	
	f_ritem->f_iflag =        (int *) c_real_item_iflag(f_ritem->f_self);
	f_ritem->f_block_name =  (char *) c_real_item_block_name(f_ritem->f_self);
	f_ritem->c_block_name = strngcopy_from_f(f_ritem->f_block_name);
	
	f_ritem->f_realvalue =  (double *) c_real_item_realvalue(f_ritem->f_self);
	return f_ritem;
}


void dealloc_f_ctl_real_item(struct f_ctl_real_item *f_ritem)
{
	free(f_ritem->c_block_name);
	
	f_ritem->f_realvalue = NULL;
	f_ritem->f_iflag = NULL;
	f_ritem->f_block_name = NULL;
	f_ritem->f_self = NULL;
	return;
}

struct f_ctl_real_array * init_f_ctl_real_array(void *(*c_load_self)(void *f_parent), 
												  void *f_parent)
{
	struct f_ctl_real_array *f_rarray = (struct f_ctl_real_array *) malloc(sizeof(struct f_ctl_real_array));
	if(f_rarray == NULL){
		printf("malloc error for f_ctl_real_array\n");
		exit(0);
	};
	f_rarray->f_self =  c_load_self(f_parent);
	
	f_rarray->f_num =         (int *)  c_real_array_num(f_rarray->f_self);
	f_rarray->f_icou =        (int *)  c_real_array_icou(f_rarray->f_self);
	f_rarray->f_block_name =  (char *) c_real_array_block_name(f_rarray->f_self);
	f_rarray->c_block_name = strngcopy_from_f(f_rarray->f_block_name);
	
	f_rarray->f_rctls =       (double *) c_real_array_r_tbl(f_rarray->f_self);
	
	/*
	printf("f_rarray->f_self %p \n", f_rarray->f_self);
	printf("f_rarray->c_block_name %s %d\n", f_rarray->c_block_name, f_rarray->f_num[0]);
	for(i=0;i<f_rarray->f_num[0];i++){
		printf("%d f_rarray->f_rctls %p %s \n", i,	f_rarray->f_rctls[i]);
	}
	*/
	return f_rarray;
}

void reflesh_f_ctl_real_array(int num_array, struct f_ctl_real_array *f_rarray)
{
	c_dealloc_real_array(f_rarray->f_self);
	f_rarray->f_num[0] = num_array;
	f_rarray->f_icou[0] = num_array;
	c_alloc_real_array(f_rarray->f_self);
	f_rarray->f_rctls = (double *) c_real_array_r_tbl(f_rarray->f_self);
/*	c_check_real_array(f_rarray->f_self); */
	return;
}

void dealloc_f_ctl_real_array(struct f_ctl_real_array *f_rarray)
{
	free(f_rarray->c_block_name);
	
	f_rarray->f_rctls = NULL;
	f_rarray->f_icou = NULL;
	f_rarray->f_num = NULL;
	f_rarray->f_block_name = NULL;
	f_rarray->f_self = NULL;
	return;
}

struct f_ctl_int_item * init_f_ctl_int_item(void *(*c_load_self)(void *f_parent), 
												void *f_parent)
{
	struct f_ctl_int_item *f_iitem = (struct f_ctl_int_item *) malloc(sizeof(struct f_ctl_int_item));
	if(f_iitem == NULL){
		printf("malloc error for f_ctl_int_item\n");
		exit(0);
	};
	f_iitem->f_self =  c_load_self(f_parent);
	
	f_iitem->f_iflag =        (int *) c_int_item_iflag(f_iitem->f_self);
	f_iitem->f_block_name =  (char *) c_int_item_block_name(f_iitem->f_self);
	f_iitem->c_block_name = strngcopy_from_f(f_iitem->f_block_name);
	
	f_iitem->f_intvalue =  (int *) c_int_item_intvalue(f_iitem->f_self);
	return f_iitem;
}


void dealloc_f_ctl_int_item(struct f_ctl_int_item *f_ritem)
{
	free(f_ritem->c_block_name);
	
	f_ritem->f_intvalue = NULL;
	f_ritem->f_iflag = NULL;
	f_ritem->f_block_name = NULL;
	f_ritem->f_self = NULL;
	return;
}

struct f_ctl_int_array * init_f_ctl_int_array(void *(*c_load_self)(void *f_parent), 
												  void *f_parent)
{
	struct f_ctl_int_array *f_iarray = (struct f_ctl_int_array *) malloc(sizeof(struct f_ctl_int_array));
	if(f_iarray == NULL){
		printf("malloc error for f_ctl_int_array\n");
		exit(0);
	};
	f_iarray->f_self =  c_load_self(f_parent);
	
	f_iarray->f_num =         (int *)  c_int_array_num(f_iarray->f_self);
	f_iarray->f_icou =        (int *)  c_int_array_icou(f_iarray->f_self);
	f_iarray->f_block_name =  (char *) c_int_array_block_name(f_iarray->f_self);
	f_iarray->c_block_name = strngcopy_from_f(f_iarray->f_block_name);
	
	f_iarray->f_ictls =       (int *) c_int_array_i_tbl(f_iarray->f_self);
	
	/*
	printf("f_iarray->f_self %p \n", f_iarray->f_self);
	printf("f_iarray->c_block_name %s %d\n", f_iarray->c_block_name, f_iarray->f_num[0]);
	for(i=0;i<f_iarray->f_num[0];i++){
		printf("%d f_iarray->f_ictls %p %d \n", i,	f_iarray->f_ictls[i]);
	}
	*/
	return f_iarray;
}

void reflesh_f_ctl_int_array(int num_array, struct f_ctl_int_array *f_iarray)
{
	c_dealloc_int_array(f_iarray->f_self);
	f_iarray->f_num[0] = num_array;
	f_iarray->f_icou[0] = num_array;
	c_alloc_int_array(f_iarray->f_self);
	f_iarray->f_ictls = (int *) c_int_array_i_tbl(f_iarray->f_self);
/*	c_check_int_array(f_iarray->f_self); */
	return;
}
void dealloc_f_ctl_int_array(struct f_ctl_int_array *f_iarray)
{
	free(f_iarray->c_block_name);
	
	f_iarray->f_ictls = NULL;
	f_iarray->f_icou = NULL;
	f_iarray->f_num = NULL;
	f_iarray->f_block_name = NULL;
	f_iarray->f_self = NULL;
	return;
}
