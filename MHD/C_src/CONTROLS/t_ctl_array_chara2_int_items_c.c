/*
//  t_ctl_array_chara2_int_items_c.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include "t_ctl_array_chara2_int_items_c.h"

extern void * c_chara2_int_item_block_name(void *f_ctl);
extern void * c_chara2_int_item_iflag(void *f_ctl);
extern void * c_chara2_int_item_charavalue1(void *f_ctl);
extern void * c_chara2_int_item_charavalue2(void *f_ctl);
extern int c_chara2_int_item_intvalue(void *f_ctl);

struct chara2_int_ctl_item * init_f_ctl_c2i_item(void *(*c_load_self)(void *f_parent),
                                            void *f_parent)
{
	struct chara2_int_ctl_item *f_c2i_item = (struct chara2_int_ctl_item *) malloc(sizeof(struct chara2_int_ctl_item));
	if(f_c2i_item == NULL){
		printf("malloc error for chara2_int_ctl_item\n");
		exit(0);
	};
	f_c2i_item->f_self =  c_load_self(f_parent);
	
	f_c2i_item->f_iflag =        (int *) c_chara2_int_item_iflag(f_c2i_item->f_self);
	char *f_charavalue =  (char *) c_chara2_int_item_block_name(f_c2i_item->f_self);
	f_c2i_item->c_block_name = strngcopy_from_f(f_charavalue);
	
	f_c2i_item->i_data =  c_chara2_int_item_intvalue(f_c2i_item->f_self);
	f_charavalue =  (char *) c_chara2_int_item_charavalue1(f_c2i_item->f_self);
	f_c2i_item->c1_tbl = strngcopy_from_f(f_charavalue);
	f_charavalue =  (char *) c_chara2_int_item_charavalue2(f_c2i_item->f_self);
	f_c2i_item->c2_tbl = strngcopy_from_f(f_charavalue);
	
	/*
	printf("f_c2i_item->f_self %p \n", f_c2i_item->f_self);
	printf("f_c2i_item->c_block_name %s \n", f_c2i_item->c_block_name);
	printf("f_c2i_item %d %s %s %lf\n", f_c2i_item->f_iflag[0],
		   f_c2i_item->c1_tbl, f_c2i_item->c2_tbl, f_c2i_item->r_data);
	*/
	return f_c2i_item;
}

struct chara2_int_clist * init_f_ctl_c2i_array(void *(*c_load_self)(void *f_parent),
                                                void *f_parent)
{
	struct chara2_int_clist *c2i_clst = init_chara2_int_clist();
	c2i_clst->f_self =  c_load_self(f_parent);
	char * ctmp1 = (char *) c_chara2_int_array_block_name(c2i_clst->f_self);
	char * ctmp2;
	c2i_clst->clist_name = strngcopy_from_f(ctmp1);
	
	int i, num;
    num = c_chara2_int_array_num(c2i_clst->f_self);
    if(num == 0) {c_alloc_chara2_int_array(num, c2i_clst->f_self);};
	
	for(i=0;i<num;i++){
		ctmp1 = (char *) c_chara2_int_array_c1_tbl(i, c2i_clst->f_self);
        ctmp2 = (char *) c_chara2_int_array_c2_tbl(i, c2i_clst->f_self);
		append_chara2_int_clist(strngcopy_from_f(ctmp1), strngcopy_from_f(ctmp2), 
								c_chara2_int_array_i_tbl(i, c2i_clst->f_self), c2i_clst);
    }
	return c2i_clst;
}

struct chara2_int_clist * init_field_label_array(void *f_self)
{
    struct chara2_int_clist *c2i_clst = init_chara2_int_clist();
    c2i_clst->f_self =  f_self;
    char * ctmp1 = (char *) c_chara2_int_array_block_name(c2i_clst->f_self);
    char * ctmp2;
    c2i_clst->clist_name = strngcopy_from_f(ctmp1);
    
    int i, num;
    num = c_chara2_int_array_num(c2i_clst->f_self);
    if(num == 0) {c_alloc_chara2_int_array(num, c2i_clst->f_self);};
    
    for(i=0;i<num;i++){
        ctmp1 = (char *) c_chara2_int_array_c1_tbl(i, c2i_clst->f_self);
        ctmp2 = (char *) c_chara2_int_array_c2_tbl(i, c2i_clst->f_self);
        append_chara2_int_clist(ctmp1, ctmp2,
                                c_chara2_int_array_i_tbl(i, c2i_clst->f_self), c2i_clst);
    }
    return c2i_clst;
}

int append_field_label_array(void *f_self, struct chara2_int_clist *c2i_clst){
    int i, i_data;
    char *ctmp1, *ctmp2;

    for(i=0;i<c_chara2_int_array_num(f_self);i++){
        ctmp1 = (char *) c_chara2_int_array_c1_tbl(i, f_self);
        ctmp2 = (char *) c_chara2_int_array_c2_tbl(i, f_self);
        i_data = c_chara2_int_array_i_tbl(i, f_self);
        append_chara2_int_clist(ctmp1, ctmp2, i_data, c2i_clst);
    }
    return count_chara2_int_clist(c2i_clst);
}

void reflesh_f_ctl_c2i_array(int num_array, struct chara2_int_clist *c2i_clst)
{
	c_dealloc_chara2_int_array(c2i_clst->f_self);
	c_alloc_chara2_int_array(num_array, c2i_clst->f_self);
	return;
}
