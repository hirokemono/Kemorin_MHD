/*
//  t_ctl_array_chara_int3_items_c.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include "t_ctl_array_chara_int3_items_c.h"

extern void * c_chara_item_clength(void *f_plt, int *length);

extern void * c_chara_int3_item_block_name(void *f_ctl);
extern void * c_chara_int3_item_iflag(void *f_ctl);
extern void * c_chara_int3_item_charavalue(void *f_ctl);
extern int    c_chara_int3_item_intvalue1(void *f_ctl);
extern int    c_chara_int3_item_intvalue2(void *f_ctl);
extern int    c_chara_int3_item_intvalue3(void *f_ctl);

struct chara_int3_ctl_item * init_f_ctl_ci3_item(void *(*c_load_self)(void *f_parent),
										  void *f_parent)
{
	struct chara_int3_ctl_item *f_ci3_item = (struct chara_int3_ctl_item *) malloc(sizeof(struct chara_int3_ctl_item));
	if(f_ci3_item == NULL){
		printf("malloc error for chara_int3_ctl_item\n");
		exit(0);
	};
	f_ci3_item->f_self =  c_load_self(f_parent);
	
	f_ci3_item->f_iflag =        (int *) c_chara_int3_item_iflag(f_ci3_item->f_self);
	char *f_block_name =  (char *) c_chara_int3_item_block_name(f_ci3_item->f_self);
	f_ci3_item->c_block_name = strngcopy_from_f(f_block_name);
	
	char * ctmp =  (char *) c_chara_int3_item_charavalue(f_ci3_item->f_self);
	f_ci3_item->c_tbl = strngcopy_from_f(ctmp);
    f_ci3_item->i_data[0] =  c_chara_int3_item_intvalue1(f_ci3_item->f_self);
    f_ci3_item->i_data[1] =  c_chara_int3_item_intvalue2(f_ci3_item->f_self);
    f_ci3_item->i_data[2] =  c_chara_int3_item_intvalue3(f_ci3_item->f_self);

	/*
	printf("f_ci3_item->f_self %p \n", f_ci3_item->f_self);
	printf("f_ci3_item->c_block_name %s \n", f_ci3_item->c_block_name);
	printf("f_ci3_item->c_tbl %d %s %d %d %d\n",
		   f_ci3_item->f_iflag[0], f_ci3_item->c_tbl, f_ci3_item->i_data[0],
		   f_ci3_item->i_data[1], f_ci3_item->i_data[2]);
	*/
	return f_ci3_item;
}

struct chara_int3_clist * init_f_ctl_ci3_array(void *(*c_load_self)(void *f_parent), 
											void *f_parent)
{
	struct chara_int3_clist * ci3_clist = init_chara_int3_clist();
	ci3_clist->f_self =  c_load_self(f_parent);
	char * ctmp = (char *) c_chara_int3_array_block_name(ci3_clist->f_self);
	ci3_clist->clist_name = strngcopy_from_f(ctmp);
	
	int i, num;
    num = c_chara_int3_array_num(ci3_clist->f_self);
    if(num == 0) {c_alloc_chara_int3_array(num, ci3_clist->f_self);};
	
	for(i=0;i<num;i++){
		ctmp = (char *) c_chara_int3_array_c_tbl(i, ci3_clist->f_self);
		append_chara_int3_clist(strngcopy_from_f(ctmp), 
								c_chara_int3_array_i1_tbl(i, ci3_clist->f_self),
								c_chara_int3_array_i2_tbl(i, ci3_clist->f_self),
								c_chara_int3_array_i3_tbl(i, ci3_clist->f_self),
								ci3_clist);
    }
	return ci3_clist;
}

void reflesh_f_ctl_ci3_array(int num_array, struct chara_int3_clist *ci3_clist)
{
	c_dealloc_chara_int3_array(ci3_clist->f_self);
	c_alloc_chara_int3_array(num_array, ci3_clist->f_self);
	return;
}
