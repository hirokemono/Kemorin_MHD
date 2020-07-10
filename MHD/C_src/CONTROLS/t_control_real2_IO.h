/*
//  t_control_real2_IO.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#ifndef t_control_real2_IO_h_
#define t_control_real2_IO_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"


struct real2_ctl_item{
	int iflag;
	
	double r_data[2];
};

struct real2_ctl_list{
    struct real2_ctl_item *r2_item;
    
    struct real2_ctl_list *_prev;
    struct real2_ctl_list *_next;
};

struct real2_clist{
	struct real2_ctl_list r2_item_head;

    char *clist_name;
    char *r1_name;
    char *r2_name;
};


/* prototypes */

struct real2_ctl_item * init_real2_ctl_item_c(void);
int read_real2_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                          struct real2_ctl_item *r2_item);
int write_real2_ctl_item_c(FILE *fp, int level, int maxlen, 
			const char *label, struct real2_ctl_item *r2_item);

void update_real2_ctl_item_c(double r1_in, double r2_in,  
                              struct real2_ctl_item *r2_item);
void set_from_real2_ctl_item_c(struct real2_ctl_item *r2_item,
                              double *r1_out, double *r2_out);

struct real2_ctl_list *find_r2_between_item_by_value1(double ref_1,
			struct real2_ctl_list *head);

struct real2_clist * init_real2_clist(void);
void clear_real2_clist(struct real2_clist *r2_clst);
void dealloc_real2_clist(struct real2_clist *r2_clst);

int count_real2_clist(struct real2_clist *r2_clst);

int read_real2_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct real2_clist *r2_clst);
int write_real2_clist(FILE *fp, int level, const char *label, 
                       struct real2_clist *r2_clst);

void append_real2_clist(double r1_in, double r2_in, struct real2_clist *r2_clst);
void del_real2_clist_by_index(int index, struct real2_clist *r2_clst);
void update_real2_clist_by_index(int index, double r1_in, double r2_in,
			struct real2_clist *r2_clst);
void set_from_real2_clist_at_index(int index, struct real2_clist *r2_clst,
			double *r1_out, double *r2_out);

void add_real2_clist_before_c_tbl(double ref_1, double ref_2, 
			struct real2_clist *r2_clst);
void add_real2_clist_after_c_tbl(double ref_1, double ref_2, 
			struct real2_clist *r2_clst);
void add_real2_clist_between_value1(double ref_1, double ref_2, 
			struct real2_clist *r2_clst);
void del_real2_clist_by_c_tbl(double ref_1, double ref_2,
			struct real2_clist *r2_clst);
void update_real2_clist_by_c_tbl(double ref_1, double ref_2, 
			double r1_in, double r2_in, struct real2_clist *r2_clst);
void set_from_real2_clist_at_c_tbl(double ref_1, double ref_2,
			struct real2_clist *r2_clst, double *r1_out, double *r2_out);

void copy_from_real2_clist(struct real2_clist *r2_clst, int num,
			double *v1, double *v2);
void copy_to_real2_clist(int num, double *v1, double *v2,
			struct real2_clist *r2_clst);
void dup_real2_clist(struct real2_clist *r2_src_clst,
            struct real2_clist *r2_tgt_clst);

#endif /* t_control_real2_IO_h_ */
