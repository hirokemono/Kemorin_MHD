/*
//  t_ctl_array_chara2_real_items_c.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include "t_ctl_array_chara2_real_items_c.h"

extern void * c_chara2_real_item_block_name(void *f_ctl);
extern void * c_chara2_real_item_iflag(void *f_ctl);
extern void * c_chara2_real_item_charavalue1(void *f_ctl);
extern void * c_chara2_real_item_charavalue2(void *f_ctl);
extern double c_chara2_real_item_realvalue(void *f_ctl);

struct chara2_real_ctl_item * init_f_ctl_c2r_item(void *(*c_load_self)(void *f_parent),
                                            void *f_parent)
{
	struct chara2_real_ctl_item *f_c2r_item = (struct chara2_real_ctl_item *) malloc(sizeof(struct chara2_real_ctl_item));
	if(f_c2r_item == NULL){
		printf("malloc error for chara2_real_ctl_item\n");
		exit(0);
	};
	f_c2r_item->f_self =  c_load_self(f_parent);
	
	f_c2r_item->f_iflag =        (int *) c_chara2_real_item_iflag(f_c2r_item->f_self);
	char *f_charavalue =  (char *) c_chara2_real_item_block_name(f_c2r_item->f_self);
	f_c2r_item->c_block_name = strngcopy_from_f(f_charavalue);
	
	f_c2r_item->r_data =  c_chara2_real_item_realvalue(f_c2r_item->f_self);
	f_charavalue =  (char *) c_chara2_real_item_charavalue1(f_c2r_item->f_self);
	f_c2r_item->c1_tbl = strngcopy_from_f(f_charavalue);
	f_charavalue =  (char *) c_chara2_real_item_charavalue2(f_c2r_item->f_self);
	f_c2r_item->c2_tbl = strngcopy_from_f(f_charavalue);
	
	/*
	printf("f_c2r_item->f_self %p \n", f_c2r_item->f_self);
	printf("f_c2r_item->c_block_name %s \n", f_c2r_item->c_block_name);
	printf("f_c2r_item %d %s %s %lf\n", f_c2r_item->f_iflag[0],
		   f_c2r_item->c1_tbl, f_c2r_item->c2_tbl, f_c2r_item->r_data);
	*/
	return f_c2r_item;
}

struct chara2_real_clist * init_f_ctl_c2r_array(void *(*c_load_self)(void *f_parent),
                                                void *f_parent)
{
	struct chara2_real_clist *c2r_clst = init_chara2_real_clist();
	c2r_clst->f_self =  c_load_self(f_parent);
	char * ctmp1 = (char *) c_chara2_real_array_block_name(c2r_clst->f_self);
	char * ctmp2;
	c2r_clst->clist_name = strngcopy_from_f(ctmp1);
	
	int i, num;
    num = c_chara2_real_array_num(c2r_clst->f_self);
    if(num == 0) {c_alloc_chara2_real_array(num, c2r_clst->f_self);};
	
	for(i=0;i<num;i++){
		ctmp1 = (char *) c_chara2_real_array_c1_tbl(i, c2r_clst->f_self);
		ctmp2 = (char *) c_chara2_real_array_c2_tbl(i, c2r_clst->f_self);
		append_chara2_real_clist(strngcopy_from_f(ctmp1), strngcopy_from_f(ctmp2), 
								c_chara2_real_array_r_tbl(i, c2r_clst->f_self), c2r_clst);
    }
	return c2r_clst;
}

void reflesh_f_ctl_c2r_array(int num_array, struct chara2_real_clist *c2r_clst)
{
	c_dealloc_chara2_real_array(c2r_clst->f_self);
	c_alloc_chara2_real_array(num_array, c2r_clst->f_self);
	return;
}
