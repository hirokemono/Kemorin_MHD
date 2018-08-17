/*
//  t_control_chara_int_IO.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#ifndef t_control_chara_int_IO_h_
#define t_control_chara_int_IO_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"


struct chara_int_ctl_item{
	int iflag;
	
	char *c_tbl;
	int i_data;
};

struct chara_int_ctl_list{
    struct chara_int_ctl_item *ci_item;
    struct maxlen_2 *mlen2;
    
    struct chara_int_ctl_list *_prev;
    struct chara_int_ctl_list *_next;
};

struct chara_int_clist{
	struct chara_int_ctl_list ci_item_head;
};


/* prototypes */

void alloc_chara_int_ctl_item_c(struct chara_int_ctl_item *ci_item);
void dealloc_chara_int_ctl_item_c(struct chara_int_ctl_item *ci_item);
int read_chara_int_ctl_item_c(char buf[LENGTHBUF], const char *label, struct chara_int_ctl_item *ci_item);
int write_chara_int_ctl_item_c(FILE *fp, int level, int maxlen[2], 
			const char *label, struct chara_int_ctl_item *ci_item);

void update_chara_int_ctl_item_c(char *c_in, int i1_in,
                              struct chara_int_ctl_item *ci_item);
void set_from_chara_int_ctl_item_c( struct chara_int_ctl_item *ci_item,
                              char *c_out, int *i1_out);


void init_chara_int_clist(struct chara_int_clist *ci_clst);
void clear_chara_int_clist(struct chara_int_clist *ci_clst);
int count_chara_int_clist(struct chara_int_clist *ci_clst);
int read_chara_int_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara_int_clist *ci_clst);
int write_chara_int_clist(FILE *fp, int level, const char *label, 
                       struct chara_int_clist *ci_clst);

void append_chara_int_clist(char *c_in, int i1_in,
                      struct chara_int_clist *ci_clst);
void del_chara_int_clist_by_index(int index, struct chara_int_clist *ci_clst);
void update_chara_int_clist_by_index(int index, char *c_in, int i1_in,
			struct chara_int_clist *ci_clst);
void set_from_chara_int_clist_at_index(int index, struct chara_int_clist *ci_clst,
			char *c_out, int *i1_out);

void del_chara_int_clist_by_c_tbl(char *ref, struct chara_int_clist *ci_clst);
void update_chara_int_clist_by_c_tbl(char *ref, char *c_in, int i1_in,
			struct chara_int_clist *ci_clst);
void set_from_chara_int_clist_at_c_tbl(char *ref, struct chara_int_clist *ci_clst,
			char *c_out, int *i1_out);

#endif /* t_control_chara_int_IO_h_ */
