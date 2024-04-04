/*
//  t_ctl_array_chara3_items_c.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include "t_ctl_array_chara3_items_c.h"

extern void * c_chara_item_clength(void *f_plt, int *length);

extern void * c_chara3_item_block_name(void *f_ctl);
extern void * c_chara3_item_iflag(void *f_ctl);
extern void * c_chara3_item_charavalue_1(void *f_ctl);
extern void * c_chara3_item_charavalue_2(void *f_ctl);
extern void * c_chara3_item_charavalue_3(void *f_ctl);

struct chara3_ctl_item * init_f_ctl_c3_item(void *(*c_load_self)(void *f_parent),
										  void *f_parent)
{
	struct chara3_ctl_item *f_c3_item = (struct chara3_ctl_item *) malloc(sizeof(struct chara3_ctl_item));
	if(f_c3_item == NULL){
		printf("malloc error for chara3_ctl_item\n");
		exit(0);
	};
	f_c3_item->f_self =  c_load_self(f_parent);
	
	f_c3_item->f_iflag =        (int *) c_chara3_item_iflag(f_c3_item->f_self);
	char *f_block_name =  (char *) c_chara3_item_block_name(f_c3_item->f_self);
	f_c3_item->c_block_name = strngcopy_from_f(f_block_name);
	
	char * ctmp =  (char *) c_chara3_item_charavalue_1(f_c3_item->f_self);
	f_c3_item->c1_tbl = strngcopy_from_f(ctmp);
	ctmp =  (char *) c_chara3_item_charavalue_2(f_c3_item->f_self);
	f_c3_item->c2_tbl = strngcopy_from_f(ctmp);
	ctmp =  (char *) c_chara3_item_charavalue_3(f_c3_item->f_self);
	f_c3_item->c3_tbl = strngcopy_from_f(ctmp);

	/*
	printf("f_c3_item->f_self %p \n", f_c3_item->f_self);
	printf("f_c3_item->c_block_name %s \n", f_c3_item->c_block_name);
	printf("f_c3_item->c_charavalue %d %s %s %s\n", 
		   f_c3_item->f_iflag[0], f_c3_item->c1_tbl,
		   f_c3_item->c2_tbl, f_c3_item->c3_tbl);
	*/
	return f_c3_item;
}

struct chara3_clist * init_f_ctl_c3_array(void *(*c_load_self)(void *f_parent), 
										  void *f_parent)
{
	struct chara3_clist * c3_clist = init_chara3_clist();
	c3_clist->f_self =  c_load_self(f_parent);
	char * ctmp1 = (char *) c_chara3_array_block_name(c3_clist->f_self);
	char * ctmp2;
	char * ctmp3;
	c3_clist->clist_name = strngcopy_from_f(ctmp1);
	
	int i, num;
    num = c_chara3_array_num(c3_clist->f_self);
    if(num == 0) {c_alloc_chara3_array(num, c3_clist->f_self);};
	
	for(i=0;i<num;i++){
		ctmp1 = (char *) c_chara3_array_c1_tbl(i, c3_clist->f_self);
		ctmp2 = (char *) c_chara3_array_c2_tbl(i, c3_clist->f_self);
		ctmp3 = (char *) c_chara3_array_c3_tbl(i, c3_clist->f_self);
		append_chara3_clist(strngcopy_from_f(ctmp1), strngcopy_from_f(ctmp2), 
							strngcopy_from_f(ctmp3), c3_clist);
    }
	return c3_clist;
}

void reflesh_f_ctl_c3_array(int num_array, struct chara3_clist *c3_clist)
{
	c_dealloc_chara3_array(c3_clist->f_self);
	c_alloc_chara3_array(num_array, c3_clist->f_self);
	return;
}
