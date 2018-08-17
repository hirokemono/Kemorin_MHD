/*
//  t_control_chara3_IO.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#ifndef t_control_chara3_IO_h_
#define t_control_chara3_IO_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"


struct chara3_ctl_item{
	
	int iflag;
	char *c1_tbl;
	char *c2_tbl;
	char *c3_tbl;
};

struct chara3_ctl_list{
    struct chara3_ctl_item *c3_item;
    struct maxlen_3 *mlen3;
    
    struct chara3_ctl_list *_prev;
    struct chara3_ctl_list *_next;
};

struct chara3_clist{
	struct chara3_ctl_list c3_item_head;
};


/* prototypes */

void alloc_chara3_ctl_item_c(struct chara3_ctl_item *c3_item);
void dealloc_chara3_ctl_item_c(struct chara3_ctl_item *c3_item);
int read_chara3_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                           struct chara3_ctl_item *c3_item);
int write_chara3_ctl_item_c(FILE *fp, int level, int maxlen[3], 
			const char *label, struct chara3_ctl_item *c3_item);

void update_chara3_ctl_item_c(char *c1_in, char *c2_in, char *c3_in,  
			struct chara3_ctl_item *c3_item);
void set_from_chara3_ctl_item_c(struct chara3_ctl_item *c3_item,
                              char *c1_out, char *c2_out, char *c3_out);


void init_chara3_clist(struct chara3_clist *c3_clst);
void clear_chara3_clist(struct chara3_clist *c3_clst);
int count_chara3_clist(struct chara3_clist *c3_clst);

int read_chara3_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                   struct chara3_clist *c3_clst);
int write_chara3_clist(FILE *fp, int level, const char *label, 
                    struct chara3_clist *c3_clst);

void append_chara3_clist(char *c1_in, char *c2_in, char *c3_in,
                      struct chara3_clist *c3_clst);
void del_chara3_clist_by_index(int index, struct chara3_clist *c3_clst);
void update_chara3_clist_by_index(int index, char *c1_in, char *c2_in, char *c3_in,
			struct chara3_clist *c3_clst);
void set_from_chara3_clist_at_index(int index, struct chara3_clist *c3_clst,
			char *c1_out, char *c2_out, char *c3_out);

void del_chara3_clist_by_c_tbl(char *ref, struct chara3_clist *c3_clst);
void update_chara3_clist_by_c_tbl(char *ref, char *c1_in, char *c2_in, char *c3_in,
			struct chara3_clist *c3_clst);
void set_from_chara3_clist_at_c_tbl(char *ref, struct chara3_clist *c3_clst,
			char *c1_out, char *c2_out, char *c3_out);

#endif /* t_control_chara3_IO_h_ */
