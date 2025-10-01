/*
//  t_ctl_array_real3_items_c.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/
#include "t_ctl_array_real3_items_c.h"


extern void * c_chara_item_clength(void *f_plt, int *length);

extern void c_alloc_real3_array(int numn, void *f_ctl);
extern void c_dealloc_real3_array(void *f_ctl);
extern void c_check_real3_array(void *f_ctl);

extern void * c_real3_item_block_name(void *f_ctl);
extern void * c_real3_item_iflag(void *f_ctl);
extern void * c_real3_item_realvalue(void *f_ctl);

struct real3_ctl_item * init_f_ctl_r3_item(void *(*c_load_self)(void *f_parent),
										   void *f_parent)
{
	struct real3_ctl_item *f_r3_item = (struct real3_ctl_item *) malloc(sizeof(struct real3_ctl_item));
	if(f_r3_item == NULL){
		printf("malloc error for real3_ctl_item\n");
		exit(0);
	};
	f_r3_item->f_self =  c_load_self(f_parent);
	
	f_r3_item->f_iflag =        (int *) c_real3_item_iflag(f_r3_item->f_self);
	char *f_block_name =  (char *) c_real3_item_block_name(f_r3_item->f_self);
	f_r3_item->c_block_name = strngcopy_from_f(f_block_name);
	
	double *f_realvalue = (double *) c_real3_item_realvalue(f_r3_item->f_self);
    f_r3_item->r_data[0] = f_realvalue[0];
    f_r3_item->r_data[1] = f_realvalue[1];
    f_r3_item->r_data[2] = f_realvalue[2];
	return f_r3_item;
}

struct real3_clist * init_f_ctl_r3_array(void *(*c_load_self)(void *f_parent),
										 void *f_parent)
{
    struct real3_clist *r3_clist = init_real3_clist();
	r3_clist->f_self =  c_load_self(f_parent);
	char *ctmp = c_real3_array_block_name(r3_clist->f_self);
	sprintf(r3_clist->clist_name,"%s", strngcopy_from_f(ctmp));
	
	int i, num;
    num = c_real3_array_num(r3_clist->f_self);
    if(num == 0) {c_alloc_real3_array(num, r3_clist->f_self);};
	
	for(i=0;i<num;i++){
		append_real3_clist(c_real3_array_r1_tbl(r3_clist->f_self, i),
						  c_real3_array_r2_tbl(r3_clist->f_self, i),
						  c_real3_array_r3_tbl(r3_clist->f_self, i), r3_clist);
    }
	return r3_clist;
}

void reflesh_f_ctl_r3_array(int num_array, struct real3_clist *r3_clist)
{
	c_dealloc_real3_array(r3_clist->f_self);
	c_alloc_real3_array(num_array, r3_clist->f_self);
	return;
}
