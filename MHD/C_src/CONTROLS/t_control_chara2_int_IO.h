/*
//  t_control_chara2_int_IO.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#ifndef T_CONTROL_CHARA2_INT_IO_H_
#define T_CONTROL_CHARA2_INT_IO_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"


struct chara2_int_ctl_item{
	void * f_self;
	int * f_iflag;
	char * c_block_name;

	char *c1_tbl;
	char *c2_tbl;
	int i_data;
};

struct chara2_int_ctl_list{
    struct chara2_int_ctl_item *c2i_item;
    
    struct chara2_int_ctl_list *_prev;
    struct chara2_int_ctl_list *_next;
};

struct chara2_int_clist{
	struct chara2_int_ctl_list c2i_item_head;

    void *f_self;

    char *clist_name;
    char *c1_name;
    char *c2_name;
    char *i1_name;
    
    int index_bc;
};

/* prototypes */

struct chara2_int_ctl_item * init_c2i_ctl_item_c(void);
void dealloc_c2i_ctl_item_c(struct chara2_int_ctl_item *c2i_item);
int read_c2i_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                        struct chara2_int_ctl_item *c2i_item);
int write_c2i_ctl_item_c(FILE *fp, int level, int maxlen[3], 
			const char *label, struct chara2_int_ctl_item *c2i_item);

void update_chara2_int_ctl_item_c(char *c1_in, char *c2_in, int i_in,  
                              struct chara2_int_ctl_item *c2i_item);
void set_from_chara2_int_ctl_item_c(struct chara2_int_ctl_item *c2i_item,
                              char *c1_out, char *c2_out, double *r_out);



struct chara2_int_clist * init_chara2_int_clist(void);
void dealloc_chara2_int_clist(struct chara2_int_clist *c2i_clst);
int count_chara2_int_clist(struct chara2_int_clist *c2i_clst);

int read_chara2_int_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara2_int_clist *c2i_clst);
int write_chara2_int_clist(FILE *fp, int level, const char *label, 
                       struct chara2_int_clist *c2i_clst);

void append_chara2_int_clist(char *c1_in, char *c2_in, int i_in,
                      struct chara2_int_clist *c2i_clst);
void del_chara2_int_clist_by_index(int index, struct chara2_int_clist *c2i_clst);
void update_chara2_int_clist_by_index(int index, char *c1_in, char *c2_in, int i_in,
			struct chara2_int_clist *c2i_clst);
void set_from_chara2_int_clist_at_index(int index, struct chara2_int_clist *c2i_clst,
			char *c1_out, char *c2_out, double *r_out);

struct chara2_int_ctl_item *chara2_int_clist_at_index(int index, struct chara2_int_clist *c2i_clst);
struct chara2_int_ctl_item *chara2_int_clist_by_ref1(char *ref1, struct chara2_int_clist *c2i_clst);

void add_chara2_int_clist_before_c_tbl(char *ref_1, char *ref_2,
			char *c1_in, char *c2_in, int i_in,
			struct chara2_int_clist *c2i_clst);
void add_chara2_int_clist_after_c_tbl(char *ref_1, char *ref_2,
			char *c1_in, char *c2_in, int i_in,
			struct chara2_int_clist *c2i_clst);
void del_chara2_int_clist_by_c_tbl(char *ref_1, char *ref_2,
			struct chara2_int_clist *c2i_clst);
void update_chara2_int_clist_by_c_tbl(char *ref_1, char *ref_2,
			char *c1_in, char *c2_in, int i_in,
			struct chara2_int_clist *c2i_clst);
void set_from_chara2_int_clist_at_c_tbl(char *ref_1, char *ref_2,
			struct chara2_int_clist *c2i_clst,
			char *c1_out, char *c2_out, double *r_out);

#endif /* T_CONTROL_CHARA2_INT_IO_H_ */
