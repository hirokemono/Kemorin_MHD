/*
//  t_ctl_array_int_real_items_c.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include "t_ctl_array_int_real_items_c.h"

extern void * c_chara_item_clength(void *f_plt, int *length);

extern void * c_int_real_item_block_name(void *f_ctl);
extern void * c_int_real_item_iflag(void *f_ctl);
extern void * c_int_real_item_intvalue(void *f_ctl);
extern void * c_int_real_item_realvalue(void *f_ctl);

struct f_ctl_ir_item * init_f_ctl_ir_item(void *(*c_load_self)(void *f_parent),
										  void *f_parent)
{
	struct f_ctl_ir_item *f_ir_item = (struct f_ctl_ir_item *) malloc(sizeof(struct f_ctl_ir_item));
	if(f_ir_item == NULL){
		printf("malloc error for f_ctl_ir_item\n");
		exit(0);
	};
	f_ir_item->f_self =  c_load_self(f_parent);
	
	f_ir_item->f_iflag =        (int *) c_int_real_item_iflag(f_ir_item->f_self);
	f_ir_item->f_block_name =  (char *) c_int_real_item_block_name(f_ir_item->f_self);
	f_ir_item->c_block_name = strngcopy_from_f(f_ir_item->f_block_name);
	
	f_ir_item->f_intvalue =   (int *)    c_int_real_item_intvalue(f_ir_item->f_self);
	f_ir_item->f_realvalue =  (double *) c_int_real_item_realvalue(f_ir_item->f_self);
	
	return f_ir_item;
}

void dealloc_f_ctl_ir_item(struct f_ctl_ir_item *f_ir_item)
{
	free(f_ir_item->c_block_name);
	
	f_ir_item->f_realvalue = NULL;
	f_ir_item->f_intvalue = NULL;
	f_ir_item->f_iflag = NULL;
	f_ir_item->f_block_name = NULL;
	f_ir_item->f_self = NULL;
	return;
}


struct int_real_clist * init_f_ctl_ir_array(void *(*c_load_self)(void *f_parent), 
											void *f_parent)
{
    struct int_real_clist *ir_clst = init_int_real_clist();
	ir_clst->f_self =  c_load_self(f_parent);
	int i;
	char *ctmp = c_int_real_array_block_name(ir_clst->f_self);
	sprintf(ir_clst->clist_name,"%s", strngcopy_from_f(ctmp));
    for(i=0;i<c_int_real_array_num(ir_clst->f_self);i++){
		append_int_real_clist(c_int_real_array_i_tbl(i, ir_clst->f_self),
							  c_int_real_array_r_tbl(i, ir_clst->f_self), ir_clst);
    }
	return ir_clst;
}

void reflesh_f_ctl_ir_array(int num_array, struct int_real_clist *ir_clst)
{
	c_dealloc_int_real_array(ir_clst->f_self);
	c_alloc_int_real_array(num_array, ir_clst->f_self);
	return;
}
