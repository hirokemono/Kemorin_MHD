/*
//  t_control_chara_int2_IO.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#ifndef t_control_chara_int2_IO_h_
#define t_control_chara_int2_IO_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"


struct chara_int2_ctl_item{
	int iflag;
	
	char *c_tbl;
	int i_data[2];
};

struct chara_int2_ctl_list{
    struct chara_int2_ctl_item *ci2_item;
    
    struct chara_int2_ctl_list *_prev;
    struct chara_int2_ctl_list *_next;
};

struct chara_int2_clist{
	struct chara_int2_ctl_list ci2_item_head;

    void *f_self;

    char *clist_name;
    char *c1_name;
    char *i1_name;
    char *i2_name;
    
    int index_bc;
};

/* prototypes */

struct chara_int2_ctl_item * init_chara_int2_ctl_item_c(void);
void dealloc_chara_int2_ctl_item_c(struct chara_int2_ctl_item *ci2_item);

void update_chara_int2_ctl_item_c(char *c_in, int i1_in, int i2_in,  
                              struct chara_int2_ctl_item *ci2_item);



struct chara_int2_clist * init_chara_int2_clist(void);
void dealloc_chara_int2_clist(struct chara_int2_clist *ci2_clst);
int count_chara_int2_clist(struct chara_int2_clist *ci2_clst);

void append_chara_int2_clist(struct chara_int2_ctl_item *ci2_input,
                             struct chara_int2_clist *ci2_clst);
void del_chara_int2_clist_by_index(int index, struct chara_int2_clist *ci2_clst);
void update_chara_int2_clist_by_index(int index, struct chara_int2_ctl_item *ci2_in,
                                      struct chara_int2_clist *ci2_clst);

struct chara_int2_ctl_item *find_chara_int2_ctl_item_by_index(int index,
                                                              struct chara_int2_clist *ci2_clst);
struct chara_int2_ctl_item *find_chara_int2_ctl_item_by_cref(char *ref,
                                                             struct chara_int2_clist *ci2_clst);
int find_chara_int2_ctl_index_by_cref(char *ref, struct chara_int2_clist *ci2_clst);

void add_chara_int2_clist_before_c_tbl(char *ref, struct chara_int2_ctl_item *ci2_input,
                                       struct chara_int2_clist *ci2_clst);
void add_chara_int2_clist_after_c_tbl(char *ref, struct chara_int2_ctl_item *ci2_input,
                                      struct chara_int2_clist *ci2_clst);
void del_chara_int2_clist_by_c_tbl(char *ref, struct chara_int2_clist *ci2_clst);
void update_chara_int2_clist_by_c_tbl(char *ref, struct chara_int2_ctl_item *ci2_in,
			struct chara_int2_clist *ci2_clst);

int count_maxlen_chara_int2_clist(const char *label, struct chara_int2_clist *ci2_clst,
                                  int mlen2[2]);

#endif /* t_control_chara_int2_IO_h_ */
