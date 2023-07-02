/*
//  t_control_c_lists.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#ifndef T_CONTROL_C_LISTS_H_
#define T_CONTROL_C_LISTS_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "skip_comment_c.h"


struct void_ctl_list{
    void *void_item;
    
    struct void_ctl_list *_prev;
    struct void_ctl_list *_next;
};

struct void_clist{
	struct void_ctl_list c_item_head;
    
    void *f_self;
    
    int iflag_use;
    char *clist_name;
    
    int index_bc;
};


/* prototypes */

struct void_clist * init_void_clist(void);
void dealloc_void_clist(struct void_clist *v_clist);
int count_void_clist(struct void_clist *v_clist);

void append_void_clist(void *void_in, struct void_clist *v_clist);
void add_void_clist_before_index(int index, void *void_in, struct void_clist *v_clist);
void add_void_clist_after_index(int index, void *void_in, struct void_clist *v_clist);
void del_void_clist_by_index(int index, struct void_clist *v_clist);

void * void_clist_at_index(int index, struct void_clist *v_clist);

#endif /* T_CONTROL_C_LISTS_H_ */
