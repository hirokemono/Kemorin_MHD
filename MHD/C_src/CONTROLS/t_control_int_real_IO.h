/*
//  t_control_int_real_IO.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#ifndef t_control_int_real_IO_h_
#define t_control_int_real_IO_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"


struct int_real_ctl_item{
	int iflag;
	
	int i_data;
	double r_data;
};

struct int_real_ctl_list{
    struct int_real_ctl_item *ir_item;
    
    struct int_real_ctl_list *_prev;
    struct int_real_ctl_list *_next;
};

struct int_real_clist{
	struct int_real_ctl_list ir_item_head;

    char *clist_name;
    char *i1_name;
    char *r1_name;
};


/* prototypes */

struct int_real_ctl_item * init_int_real_ctl_item_c(void);
int read_int_real_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                          struct int_real_ctl_item *ir_item);
int write_int_real_ctl_item_c(FILE *fp, int level, int maxlen, 
			const char *label, struct int_real_ctl_item *ir_item);

void update_int_real_ctl_item_c(int i1_in, double r2_in,  
			struct int_real_ctl_item *ir_item);
void set_from_int_real_ctl_item_c(struct int_real_ctl_item *ir_item,
			int *i1_out, double *r2_out);



struct int_real_clist * init_int_real_clist(void);
void dealloc_int_real_clist(struct int_real_clist *ir_clst);
int count_int_real_clist(struct int_real_clist *ir_clst);

int read_int_real_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct int_real_clist *ir_clst);
int write_int_real_clist(FILE *fp, int level, const char *label, 
                       struct int_real_clist *ir_clst);


void append_int_real_clist(int i1_in, double r2_in, struct int_real_clist *ir_clst);
void del_int_real_clist_by_index(int index, struct int_real_clist *ir_clst);
void update_int_real_clist_by_index(int index, int i1_in, double r2_in,
			struct int_real_clist *ir_clst);
void set_from_int_real_clist_at_index(int index, struct int_real_clist *ir_clst,
			int *i1_out, double *r2_out);

void add_int_real_clist_before_c_tbl(int iref_1, double ref_2, 
			int i1_in, double r2_in, struct int_real_clist *ir_clst);
void add_int_real_clist_after_c_tbl(int iref_1, double ref_2, 
			int i1_in, double r2_in, struct int_real_clist *ir_clst);
void del_int_real_clist_by_c_tbl(int iref_1, double ref_2,
			struct int_real_clist *ir_clst);
void update_int_real_clist_by_c_tbl(int iref_1, double ref_2, 
			int i1_in, double r2_in, struct int_real_clist *ir_clst);
void set_from_int_real_clist_at_c_tbl(int iref_1, double ref_2,
			struct int_real_clist *ir_clst, int *i1_out, double *r2_out);

void copy_from_int_real_clist(struct int_real_clist *ir_clst, int num,
			int *iv1, double *v2);
void copy_to_int_real_clist(int num, int *iv1, double *v2,
			struct int_real_clist *ir_clst);

#endif /* t_control_int_real_IO_h_ */
