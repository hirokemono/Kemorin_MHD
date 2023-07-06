/*
//  t_control_c_lists_w_file.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#ifndef T_CONTROL_C_LISTS_W_FILE_H_
#define T_CONTROL_C_LISTS_W_FILE_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "skip_comment_c.h"


struct void_file_ctl_list{
	void *void_item;
	char *file_name;
	char *list_label;
    
    struct void_file_ctl_list *_prev;
    struct void_file_ctl_list *_next;
};

struct void_file_clist{
	struct void_file_ctl_list c_item_head;
    
    void *f_parent;
    
    int iflag_use;
    char *clist_name;
	int icount;
	
    int index_bc;
};


/* prototypes */

struct void_file_clist * init_void_file_clist(char *label);
void dealloc_void_file_clist(struct void_file_clist *vf_clist);
int count_void_file_clist(struct void_file_clist *vf_clist);

void append_void_file_clist(char *fname_in, void *void_in, 
							struct void_file_clist *vf_clist);

void add_void_file_clist_before_index(int index, char *fname_in, void *void_in,
									  struct void_file_clist *vf_clist);
void add_void_file_clist_after_index(int index, char *fname_in, void *void_in, 
									 struct void_file_clist *vf_clist);
void del_void_file_clist_by_index(int index, struct void_file_clist *vf_clist);

void add_void_file_clist_before_c_tbl(char *ref, char *fname_in, void *void_in, 
									  struct void_file_clist *vf_clist);
void add_void_file_clist_after_c_tbl(char *ref, char *fname_in, void *void_in,
									 struct void_file_clist *vf_clist);
void del_void_file_clist_by_c_tbl(char *ref, struct void_file_clist *vf_clist);

int find_void_file_clist_index_by_c_tbl(char *ref, struct void_file_clist *vf_clist);

void replace_void_file_clist_at_index(int index, char *fname_in, void *void_in, 
									  struct void_file_clist *vf_clist);
void * void_file_clist_at_index(int index, struct void_file_clist *vf_clist);
char * void_file_name_clist_at_index(int index, struct void_file_clist *vf_clist);
char * void_file_clist_label_at_index(int index, struct void_file_clist *vf_clist);

#endif /* T_CONTROL_C_LISTS_W_FILE_H_ */
