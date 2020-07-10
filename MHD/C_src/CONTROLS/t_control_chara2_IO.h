/*
//  t_control_chara2_IO.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#ifndef t_control_chara2_IO_h_
#define t_control_chara2_IO_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"


struct chara2_ctl_item{
	
	int iflag;
	char *c1_tbl;
	char *c2_tbl;
};

struct chara2_ctl_list{
    struct chara2_ctl_item *c2_item;
    
    struct chara2_ctl_list *_prev;
    struct chara2_ctl_list *_next;
};

struct chara2_clist{
	struct chara2_ctl_list c2_item_head;

    char *clist_name;
    char *c1_name;
    char *c2_name;
};


/* prototypes */

struct chara2_ctl_item * init_chara2_ctl_item_c(void);
void dealloc_chara2_ctl_item_c(struct chara2_ctl_item *c2_item);
int read_chara2_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                           struct chara2_ctl_item *c2_item);
int write_chara2_ctl_item_c(FILE *fp, int level, int maxlen[2], 
			const char *label, struct chara2_ctl_item *c2_item);

void update_chara2_ctl_item_c(char *c1_in, char *c2_in, 
			struct chara2_ctl_item *c2_item);
void set_from_chara2_ctl_item_c(struct chara2_ctl_item *c2_item,
			char *c1_out, char *c2_out);


struct chara2_clist * init_chara2_clist(void);
void dealloc_chara2_clist(struct chara2_clist *c2_clst);
int count_chara2_clist(struct chara2_clist *c2_clst);

int read_chara2_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                   struct chara2_clist *c2_clst);
int write_chara2_clist(FILE *fp, int level, const char *label, 
                    struct chara2_clist *c2_clst);

void append_chara2_clist(char *c1_in, char *c2_in,
                      struct chara2_clist *c2_clst);
void del_chara2_clist_by_index(int index, struct chara2_clist *c2_clst);
void update_chara2_clist_by_index(int index, char *c1_in, char *c2_in,
			struct chara2_clist *c2_clst);
void set_from_chara2_clist_at_index(int index, struct chara2_clist *c2_clst,
			char *c1_out, char *c2_out);

void add_chara2_clist_before_c_tbl(char *ref_1, char *ref_2, char *c1_in, char *c2_in,
			struct chara2_clist *c2_clst);
void add_chara2_clist_after_c_tbl(char *ref_1, char *ref_2, char *c1_in, char *c2_in,
			struct chara2_clist *c2_clst);
void del_chara2_clist_by_c_tbl(char *ref_1, char *ref_2, struct chara2_clist *c2_clst);
void update_chara2_clist_by_c_tbl(char *ref_1, char *ref_2, char *c1_in, char *c2_in,
			struct chara2_clist *c2_clst);
void set_from_chara2_clist_at_c_tbl(char *ref_1, char *ref_2,
			struct chara2_clist *c2_clst, char *c1_out, char *c2_out);

#endif /* t_control_chara2_IO_h_ */
