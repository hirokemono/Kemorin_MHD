/*
//  t_ctl_array_single_items_c.c
//  
//
//  Created by Hiroaki Matsui on 2020/06/14.
*/

#include "t_ctl_array_single_items_c.h"

extern void * c_chara_item_block_name(void *f_ctl);
extern void * c_chara_item_iflag(void *f_ctl);
extern void * c_chara_item_charavalue(void *f_ctl);

extern void * c_real_item_block_name(void *f_ctl);
extern void * c_real_item_iflag(void *f_ctl);
extern double c_real_item_realvalue(void *f_ctl);

extern void * c_int_item_block_name(void *f_ctl);
extern void * c_int_item_iflag(void *f_ctl);
extern int c_int_item_intvalue(void *f_ctl);

extern void * c_chara_item_clength(void *f_ctl, int *length);

char * strngcopy_from_f(char * f_char)
{
	char *c_char;
	int f_charlength[1];
	c_chara_item_clength(f_char, f_charlength);
	c_char = alloc_string((long) f_charlength[0]);
	strngcopy_w_length(c_char, f_charlength[0], f_char);
	return c_char;
}

struct f_ctl_chara_item * init_f_ctl_chara_item(void *(*c_load_self)(void *f_parent), 
												void *f_parent)
{
	struct f_ctl_chara_item *f_citem = (struct f_ctl_chara_item *) malloc(sizeof(struct f_ctl_chara_item));
	if(f_citem == NULL){
		printf("malloc error for f_ctl_chara_item\n");
		exit(0);
	};
	f_citem->f_self =  c_load_self(f_parent);
	
	f_citem->f_iflag =        (int *) c_chara_item_iflag(f_citem->f_self);
	char *f_charavalue =  (char *) c_chara_item_block_name(f_citem->f_self);
	f_citem->c_block_name = strngcopy_from_f(f_charavalue);
	
	f_charavalue =  (char *) c_chara_item_charavalue(f_citem->f_self);
	f_citem->c_charavalue = strngcopy_from_f(f_charavalue);
	
	return f_citem;
}


void dealloc_f_ctl_chara_item(struct f_ctl_chara_item *f_citem)
{
	free(f_citem->c_charavalue);
	free(f_citem->c_block_name);
	
	f_citem->f_iflag = NULL;
	f_citem->f_self = NULL;
	return;
}

struct chara_clist * init_f_ctl_chara_array(void *(*c_load_self)(void *f_parent), 
											void *f_parent)
{
	struct chara_clist *c_clist = init_chara_clist();
	c_clist->f_self =  c_load_self(f_parent);
	
	char *ctmp =  (char *) c_chara_array_block_name(c_clist->f_self);
	c_clist->clist_name = strngcopy_from_f(ctmp);
	
	int i, num;
    num = c_chara_array_num(c_clist->f_self);
    if(num == 0) {c_alloc_chara_array(num, c_clist->f_self);};
	
	for(i=0;i<num;i++){
		ctmp = (char *) c_chara_array_c_tbl(i, c_clist->f_self);
		append_chara_clist(strngcopy_from_f(ctmp), c_clist);
	};
	return c_clist;
}

void reflesh_f_ctl_chara_array(int num_array, struct chara_clist *c_clist)
{
	c_dealloc_chara_array(c_clist->f_self);
	c_alloc_chara_array(num_array, c_clist->f_self);
	return;
}

struct f_ctl_real_item * init_f_ctl_real_item(void *(*c_load_self)(void *f_parent), 
												void *f_parent)
{
	struct f_ctl_real_item *f_ritem = (struct f_ctl_real_item *) malloc(sizeof(struct f_ctl_real_item));
	if(f_ritem == NULL){
		printf("malloc error for f_ctl_real_item\n");
		exit(0);
	};
	f_ritem->f_self =  c_load_self(f_parent);
	
	f_ritem->f_iflag =        (int *) c_real_item_iflag(f_ritem->f_self);
	char *f_block_name =  (char *) c_real_item_block_name(f_ritem->f_self);
	f_ritem->c_block_name = strngcopy_from_f(f_block_name);
	
	f_ritem->c_realvalue = c_real_item_realvalue(f_ritem->f_self);
	return f_ritem;
}


void dealloc_f_ctl_real_item(struct f_ctl_real_item *f_ritem)
{
	free(f_ritem->c_block_name);
	
	f_ritem->f_iflag = NULL;
	f_ritem->f_self = NULL;
	return;
}

struct real_clist * init_f_ctl_real_array(void *(*c_load_self)(void *f_parent), 
										  void *f_parent)
{
	struct real_clist *r_clist = init_real_clist();
	r_clist->f_self =  c_load_self(f_parent);

    char *ctmp = c_real_array_block_name(r_clist->f_self);
    sprintf(r_clist->clist_name,"%s", strngcopy_from_f(ctmp));

	int i, num;
    num = c_real_array_num(r_clist->f_self);
    if(num == 0) {c_alloc_real_array(num, r_clist->f_self);};
    for(i=0;i<num;i++){
		append_real_clist(c_real_array_r_tbl(i,r_clist->f_self), r_clist);
    }
	return r_clist;
}

void reflesh_f_ctl_real_array(int num_array, struct real_clist *r_clist)
{
	c_dealloc_real_array(r_clist->f_self);
	c_alloc_real_array(num_array, r_clist->f_self);
	return;
}

struct f_ctl_int_item * init_f_ctl_int_item(void *(*c_load_self)(void *f_parent), 
												void *f_parent)
{
	struct f_ctl_int_item *f_iitem = (struct f_ctl_int_item *) malloc(sizeof(struct f_ctl_int_item));
	if(f_iitem == NULL){
		printf("malloc error for f_ctl_int_item\n");
		exit(0);
	};
	f_iitem->f_self =  c_load_self(f_parent);
	
	f_iitem->f_iflag =    (int *) c_int_item_iflag(f_iitem->f_self);
    char *f_block_name =  (char *) c_int_item_block_name(f_iitem->f_self);
	f_iitem->c_block_name = strngcopy_from_f(f_block_name);
	
	f_iitem->c_intvalue = c_int_item_intvalue(f_iitem->f_self);
	return f_iitem;
}


void dealloc_f_ctl_int_item(struct f_ctl_int_item *f_ritem)
{
	free(f_ritem->c_block_name);
    f_ritem->f_iflag = NULL;
	f_ritem->f_self = NULL;
	return;
}

struct int_clist * init_f_ctl_int_array(void *(*c_load_self)(void *f_parent), 
												  void *f_parent)
{
	struct int_clist *i_clist = init_int_clist();
	i_clist->f_self =  c_load_self(f_parent);
	char *ctmp =  (char *) c_int_array_block_name(i_clist->f_self);
	sprintf(i_clist->clist_name,"%s", strngcopy_from_f(ctmp));
	
	int i, num;
    num = c_int_array_num(i_clist->f_self);
    if(num == 0) {c_alloc_int_array(num, i_clist->f_self);};
	
	for(i=0;i<c_int_array_num(i_clist->f_self);i++){
		append_int_clist(c_int_array_i_tbl(i,i_clist->f_self), i_clist);
    }
	return i_clist;
}

void reflesh_f_ctl_int_array(int num_array, struct int_clist *i_clist)
{
	c_dealloc_int_array(i_clist->f_self);
	c_alloc_int_array(num_array, i_clist->f_self);
	return;
}
