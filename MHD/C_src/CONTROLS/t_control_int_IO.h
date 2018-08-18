/*
//  t_control_int_IO.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#ifndef t_control_int_IO_h_
#define t_control_int_IO_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"


struct int_ctl_item{
	int iflag;
	int i_data;
};

struct int_ctl_list{
    struct int_ctl_item *i_item;
    
    struct int_ctl_list *_prev;
    struct int_ctl_list *_next;
};

struct int_clist{
	struct int_ctl_list i_item_head;
};


/* prototypes */

void init_int_ctl_item_c(struct int_ctl_item *i_item);
int read_integer_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                          struct int_ctl_item *i_item);
int write_integer_ctl_item_c(FILE *fp, int level, int maxlen, 
			const char *label, struct int_ctl_item *i_item);

void update_int_ctl_item_c(int index, struct int_ctl_item *i_item);
int set_from_int_ctl_item_c(struct int_ctl_item *i_item);


void init_int_clist(struct int_clist *i_clst);
void clear_int_clist(struct int_clist *i_clst);
int count_int_clist(struct int_clist *i_clst);

int read_int_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct int_clist *i_clst);
int write_int_clist(FILE *fp, int level, const char *label, 
                       struct int_clist *i_clst);

void append_int_clist(int i1_in, struct int_clist *i_clst);
void del_int_clist_by_index(int index, struct int_clist *i_clst);
void update_int_clist_by_index(int index, int i1_in, struct int_clist *i_clst);
void set_from_int_clist_at_index(int index, struct int_clist *i_clst, int *i1_out);

void add_int_clist_before_c_tbl(int iref, int i1_in, struct int_clist *i_clst);
void add_int_clist_after_c_tbl(int iref, int i1_in, struct int_clist *i_clst);
void del_int_clist_by_c_tbl(int iref, struct int_clist *i_clst);
void update_int_clist_by_c_tbl(int iref, int i1_in, struct int_clist *i_clst);
void set_from_int_clist_at_c_tbl(int iref, struct int_clist *i_clst, int *i1_out);

void copy_from_int_clist(struct int_clist *i_clst, int num, int *iv1);
void copy_to_int_clist(int num, int *iv1, struct int_clist *i_clst);

#endif /* t_control_int_IO_h_ */
