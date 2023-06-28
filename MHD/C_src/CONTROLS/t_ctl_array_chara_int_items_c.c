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


struct chara_int_clist * init_f_ctl_ci_array(void *(*c_load_self)(void *f_parent), 
											void *f_parent)
{
	struct chara_int_clist * ci_clist = init_chara_int_clist();
	ci_clist->f_self =  c_load_self(f_parent);
	int i;
	char * ctmp = (char *) c_chara_int_array_block_name(ci_clist->f_self);
	ci_clist->clist_name = strngcopy_from_f(ctmp);
	for(i=0;i<c_chara_int_array_num(ci_clist->f_self);i++){
		ctmp = (char *) c_chara_int_array_c_tbl(i, ci_clist->f_self);
		append_chara_int_clist(strngcopy_from_f(ctmp), 
							   c_chara_int_array_i_tbl(i, ci_clist->f_self), ci_clist);
    }
	return ci_clist;
}

void reflesh_f_ctl_ci_array(int num_array, struct chara_int_clist *ci_clist)
{
	c_dealloc_chara_int_array(ci_clist->f_self);
	c_alloc_chara_int_array(num_array, ci_clist->f_self);
	return;
}
