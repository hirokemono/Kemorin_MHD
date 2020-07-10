/*
//  t_control_int2_IO.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#ifndef t_control_int2_IO_h_
#define t_control_int2_IO_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"


struct int2_ctl_item{
	int iflag;
	
	int i_data[2];
};

struct int2_ctl_list{
    struct int2_ctl_item *i2_item;
    
    struct int2_ctl_list *_prev;
    struct int2_ctl_list *_next;
};

struct int2_clist{
	struct int2_ctl_list i2_item_head;

    char *clist_name;
    char *i1_name;
    char *i2_name;
};


/* prototypes */

struct int2_ctl_item * init_int2_ctl_item_c(void);
int read_int2_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                          struct int2_ctl_item *i2_item);
int write_int2_ctl_item_c(FILE *fp, int level, int maxlen, 
			const char *label, struct int2_ctl_item *i2_item);

void update_int2_ctl_item_c(int i1_in, int i2_in,  
                              struct int2_ctl_item *i2_item);
void set_from_int2_ctl_item_c(struct int2_ctl_item *i2_item,
                              int *i1_out, int *i2_out);



struct int2_clist * init_int2_clist(void);
void dealloc_int2_clist(struct int2_clist *i2_clst);
int count_int2_clist(struct int2_clist *i2_clst);

int read_int2_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct int2_clist *i2_clst);
int write_int2_clist(FILE *fp, int level, const char *label, 
                       struct int2_clist *i2_clst);

void append_int2_clist(int i1_in, int i2_in, struct int2_clist *i2_clst);
void del_int2_clist_by_index(int index, struct int2_clist *i2_clst);
void update_int2_clist_by_index(int index, int i1_in, int i2_in,
			struct int2_clist *i2_clst);
void set_from_int2_clist_at_index(int index, struct int2_clist *i2_clst,
			int *i1_out, int *i2_out);

void add_int2_clist_before_c_tbl(int iref_1, int iref_2, 
			int i1_in, int i2_in, struct int2_clist *i2_clst);
void add_int2_clist_after_c_tbl(int iref_1, int iref_2, 
			int i1_in, int i2_in, struct int2_clist *i2_clst);
void del_int2_clist_by_c_tbl(int iref_1, int iref_2,
			struct int2_clist *i2_clst);
void update_int2_clist_by_c_tbl(int iref_1, int iref_2, 
			int i1_in, int i2_in, struct int2_clist *i2_clst);
void set_from_int2_clist_at_c_tbl(int iref_1, int iref_2,
			struct int2_clist *i2_clst, int *i1_out, int *i2_out);

void copy_from_int2_clist(struct int2_clist *i2_clst, int num,
			int *iv1, int *iv2);
void copy_to_int2_clist(int num, int *iv1, int *iv2,
			struct int2_clist *i2_clst);

#endif /* t_control_int2_IO_h_ */
