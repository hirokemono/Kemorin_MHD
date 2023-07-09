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
