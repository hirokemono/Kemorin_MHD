/*
//  t_ctl_array_int2_items_c.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/
#include "t_ctl_array_int2_items_c.h"


extern void * c_chara_item_clength(void *f_plt, int *length);

extern void c_alloc_int2_array(int numn, void *f_ctl);
extern void c_dealloc_int2_array(void *f_ctl);
extern void c_check_int2_array(void *f_ctl);

extern void * c_int2_item_block_name(void *f_ctl);
extern void * c_int2_item_iflag(void *f_ctl);
extern void * c_int2_item_intvalue(void *f_ctl);

struct int2_ctl_item * init_f_ctl_i2_item(void *(*c_load_self)(void *f_parent),
										  void *f_parent)
{
	struct int2_ctl_item *f_i2_item = (struct int2_ctl_item *) malloc(sizeof(struct int2_ctl_item));
	if(f_i2_item == NULL){
		printf("malloc error for int2_ctl_item\n");
		exit(0);
	};
	f_i2_item->f_self =  c_load_self(f_parent);
	
	f_i2_item->f_iflag =        (int *) c_int2_item_iflag(f_i2_item->f_self);
	char *f_block_name =  (char *) c_int2_item_block_name(f_i2_item->f_self);
	f_i2_item->c_block_name = strngcopy_from_f(f_block_name);
	
	int *f_intvalue =   (int *) c_int2_item_intvalue(f_i2_item->f_self);
    f_i2_item->i_data[0] = f_intvalue[0];
    f_i2_item->i_data[1] = f_intvalue[1];
	return f_i2_item;
}

struct int2_clist * init_f_ctl_i2_array(void *(*c_load_self)(void *f_parent),
											void *f_parent)
{
    struct int2_clist *i2_clst = init_int2_clist();
	i2_clst->f_self =  c_load_self(f_parent);
	char *ctmp = c_int2_array_block_name(i2_clst->f_self);
	sprintf(i2_clst->clist_name,"%s", strngcopy_from_f(ctmp));
	
	int i, num;
    num = c_int2_array_num(i2_clst->f_self);
    if(num == 0) {c_alloc_int2_array(num, i2_clst->f_self);};
	
	for(i=0;i<num;i++){
		append_int2_clist(c_int2_array_i1_tbl(i2_clst->f_self, i),
						  c_int2_array_i2_tbl(i2_clst->f_self, i), i2_clst);
    }
	return i2_clst;
}

void reflesh_f_ctl_i2_array(int num_array, struct int2_clist *i2_clst)
{
	c_dealloc_int2_array(i2_clst->f_self);
	c_alloc_int2_array(num_array, i2_clst->f_self);
	return;
}
