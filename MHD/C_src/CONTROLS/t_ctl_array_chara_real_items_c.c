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
extern double c_chara_real_item_realvalue(void *f_ctl);

extern void * c_real2_item_block_name(void *f_ctl);
extern void * c_real2_item_iflag(void *f_ctl);
extern double c_real2_item_realvalue1(void *f_ctl);
extern double c_real2_item_realvalue2(void *f_ctl);



struct chara_real_ctl_item * init_f_ctl_cr_item(void *(*c_load_self)(void *f_parent),
                                                void *f_parent)
{
	struct chara_real_ctl_item *f_cr_item = (struct chara_real_ctl_item *) malloc(sizeof(struct chara_real_ctl_item));
	if(f_cr_item == NULL){
		printf("malloc error for chara_real_ctl_item\n");
		exit(0);
	};
	f_cr_item->f_self =  c_load_self(f_parent);
	
	f_cr_item->f_iflag =        (int *) c_chara_real_item_iflag(f_cr_item->f_self);
	char *f_charavalue =  (char *) c_chara_real_item_block_name(f_cr_item->f_self);
	f_cr_item->c_block_name = strngcopy_from_f(f_charavalue);
	
	f_cr_item->r_data =  c_chara_real_item_realvalue(f_cr_item->f_self);
	f_charavalue =  (char *) c_chara_real_item_charavalue(f_cr_item->f_self);
	f_cr_item->c_tbl = strngcopy_from_f(f_charavalue);
	
	/*
	printf("f_cr_item->f_self %p \n", f_cr_item->f_self);
	printf("f_cr_item->c_block_name %s \n", f_cr_item->c_block_name);
	printf("f_cr_item->c_tbl %d %s %le\n",
		   f_cr_item->f_iflag[0], f_cr_item->c_tbl, f_cr_item->r_data);
	*/
	return f_cr_item;
}


struct chara_real_clist * init_f_ctl_cr_array(void *(*c_load_self)(void *f_parent), 
											void *f_parent)
{
	struct chara_real_clist *cr_clst = init_chara_real_clist();
	cr_clst->f_self =  c_load_self(f_parent);
	char * ctmp = (char *) c_chara_real_array_block_name(cr_clst->f_self);
	cr_clst->clist_name = strngcopy_from_f(ctmp);
	
	int i, num;
    num = c_chara_real_array_num(cr_clst->f_self);
    if(num == 0) {c_alloc_chara_real_array(num, cr_clst->f_self);};
	
	for(i=0;i<num;i++){
		ctmp = (char *) c_chara_real_array_c_tbl(i, cr_clst->f_self);
		append_chara_real_clist(strngcopy_from_f(ctmp), 
							   c_chara_real_array_r_tbl(i, cr_clst->f_self), cr_clst);
    }
	return cr_clst;
}

void reflesh_f_ctl_cr_array(int num_array, struct chara_real_clist *cr_clst)
{
	c_dealloc_chara_real_array(cr_clst->f_self);
	c_alloc_chara_real_array(num_array, cr_clst->f_self);
	return;
}



struct real2_ctl_item * init_f_ctl_r2_item(void *(*c_load_self)(void *f_parent),
                                           void *f_parent)
{
	struct real2_ctl_item *f_r2_item = (struct real2_ctl_item *) malloc(sizeof(struct real2_ctl_item));
	if(f_r2_item == NULL){
		printf("malloc error for real2_ctl_item\n");
		exit(0);
	};
	f_r2_item->f_self =  c_load_self(f_parent);
	
	f_r2_item->f_iflag =        (int *) c_real2_item_iflag(f_r2_item->f_self);
	char *f_charavalue =  (char *) c_real2_item_block_name(f_r2_item->f_self);
	f_r2_item->c_block_name = strngcopy_from_f(f_charavalue);
	
	f_r2_item->r_data[0] =  c_real2_item_realvalue1(f_r2_item->f_self);
	f_r2_item->r_data[1] =  c_real2_item_realvalue2(f_r2_item->f_self);
	
	/*
	printf("f_r2_item->f_self %p \n", f_r2_item->f_self);
	printf("f_r2_item->c_block_name %s \n", f_r2_item->c_block_name);
	printf("f_r2_item->c_tbl %d %le %le\n",
		   f_r2_item->f_iflag[0], f_r2_item->r_data[0], f_r2_item->r_data[1]);
	*/
	return f_r2_item;
}

struct real2_clist * init_f_ctl_r2_array(void *(*c_load_self)(void *f_parent), 
											void *f_parent)
{
	struct real2_clist *r2_clst = init_real2_clist();
	r2_clst->f_self =  c_load_self(f_parent);
	char * ctmp = (char *) c_real2_array_block_name(r2_clst->f_self);
	r2_clst->clist_name = strngcopy_from_f(ctmp);
	
	int i, num;
    num = c_real2_array_num(r2_clst->f_self);
    if(num == 0) {c_alloc_real2_array(num, r2_clst->f_self);};
	
	for(i=0;i<num;i++){
		append_real2_clist(c_real2_array_r1_tbl(i, r2_clst->f_self),
                           c_real2_array_r2_tbl(i, r2_clst->f_self), r2_clst);
    }
	return r2_clst;
}

void reflesh_f_ctl_r2_array(int num_array, struct real2_clist *r2_clst)
{
	c_dealloc_real2_array(r2_clst->f_self);
	c_alloc_real2_array(num_array, r2_clst->f_self);
	return;
}

