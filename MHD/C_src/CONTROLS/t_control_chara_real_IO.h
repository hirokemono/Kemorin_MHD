/*
//  t_control_chara_real_IO.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#ifndef t_control_chara_real_IO_h_
#define t_control_chara_real_IO_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"


struct chara_real_ctl_item{
	int iflag;
	
	char *c_tbl;
	double r_data;
};

struct chara_real_ctl_list{
    struct chara_real_ctl_item *cr_item;
    struct maxlen_2 *mlen2;
    
    struct chara_real_ctl_list *_prev;
    struct chara_real_ctl_list *_next;
};

struct chara_real_clist{
	struct chara_real_ctl_list cr_item_head;
};

/* prototypes */

void alloc_chara_real_ctl_item_c(struct chara_real_ctl_item *cr_item);
void dealloc_chara_real_ctl_item_c(struct chara_real_ctl_item *cr_item);
int read_chara_real_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                               struct chara_real_ctl_item *cr_item);
int write_chara_real_ctl_item_c(FILE *fp, int level, int maxlen[2], 
			const char *label, struct chara_real_ctl_item *cr_item);

void update_chara_real_ctl_item_c(char *c_in, double r_in,  
                              struct chara_real_ctl_item *cr_item);
void set_from_chara_real_ctl_item_c(struct chara_real_ctl_item *cr_item,
                              char *c_out, double *r_out);



void init_chara_real_clist(struct chara_real_clist *ci2_clst);
void clear_chara_real_clist(struct chara_real_clist *ci2_clst);
int count_chara_real_clist(struct chara_real_clist *ci2_clst);

int read_chara_real_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara_real_clist *ci2_clst);
int write_chara_real_clist(FILE *fp, int level, const char *label, 
                       struct chara_real_clist *ci2_clst);

void append_chara_real_clist(char *c_in, double r_in,
                      struct chara_real_clist *ci2_clst);
void del_chara_real_clist_by_index(int index, struct chara_real_clist *ci2_clst);
void update_chara_real_clist_by_index(int index, char *c_in, double r_in,
			struct chara_real_clist *ci2_clst);
void set_from_chara_real_clist_at_index(int index, struct chara_real_clist *ci2_clst,
			char *c_out, double *r_out);

void add_chara_real_clist_before_c_tbl(char *ref, char *c_in, double r_in, struct chara_real_clist *ci2_clst);
void add_chara_real_clist_after_c_tbl(char *ref, char *c_in, double r_in, struct chara_real_clist *ci2_clst);
void del_chara_real_clist_by_c_tbl(char *ref, struct chara_real_clist *ci2_clst);
void update_chara_real_clist_by_c_tbl(char *ref, char *c_in, double r_in,
			struct chara_real_clist *ci2_clst);
void set_from_chara_real_clist_at_c_tbl(char *ref, struct chara_real_clist *ci2_clst,
			char *c_out, double *r_out);

#endif /* t_control_chara_real_IO_h_ */
