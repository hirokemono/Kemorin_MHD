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
	char *list_label;
    
    struct void_ctl_list *_prev;
    struct void_ctl_list *_next;
};

struct void_clist{
	struct void_ctl_list c_item_head;
    
    void *f_parent;
    
    int iflag_use;
    char *clist_name;
	int icount;
	
    int index_bc;
};


/* prototypes */

struct void_clist * init_void_clist(char *label);
void dealloc_void_clist(struct void_clist *v_clist);
int count_void_clist(struct void_clist *v_clist);

void append_void_clist(void *void_in, struct void_clist *v_clist);

void add_void_clist_before_index(int index, void *void_in, struct void_clist *v_clist);
void add_void_clist_after_index(int index, void *void_in, struct void_clist *v_clist);
void del_void_clist_by_index(int index, struct void_clist *v_clist);

void add_void_clist_before_c_tbl(char *ref, void *void_in, struct void_clist *v_clist);
void add_void_clist_after_c_tbl(char *ref, void *void_in, struct void_clist *v_clist);
void del_void_clist_by_c_tbl(char *ref, struct void_clist *v_clist);

int find_void_clist_index_by_c_tbl(char *ref, struct void_clist *v_clist);

void replace_void_clist_at_index(int index, void *void_in, struct void_clist *v_clist);
void * void_clist_at_index(int index, struct void_clist *v_clist);
char * void_clist_label_at_index(int index, struct void_clist *v_clist);

#endif /* T_CONTROL_C_LISTS_H_ */
