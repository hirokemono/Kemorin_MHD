/*
//  t_control_real_IO.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#ifndef t_control_real_IO_h_
#define t_control_real_IO_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"


struct real_ctl_item{
	int iflag;
	double r_data;
};

struct real_ctl_list{
    struct real_ctl_item *r_item;
    
    struct real_ctl_list *_prev;
    struct real_ctl_list *_next;
};

struct real_clist{
	struct real_ctl_list r_item_head;

    char *clist_name;
    char *r1_name;
};


/* prototypes */

struct real_ctl_item * init_real_ctl_item_c(void);
int read_real_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                          struct real_ctl_item *r_item);
int write_real_ctl_item_c(FILE *fp, int level, int maxlen, 
			const char *label, struct real_ctl_item *r_item);

void update_real_ctl_item_c(double r1_in, struct real_ctl_item *r_item);
void set_from_real_ctl_item_c(struct real_ctl_item *r_item, double *r1_out);


struct real_clist * init_real_clist(void);
void dealloc_real_clist(struct real_clist *r_clst);
int count_real_clist(struct real_clist *r_clst);

int read_real_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct real_clist *r_clst);
int write_real_clist(FILE *fp, int level, const char *label, 
                       struct real_clist *r_clst);

void append_real_clist(double r1_in, struct real_clist *r_clst);
void del_real_clist_by_index(int index, struct real_clist *r_clst);
void update_real_clist_by_index(int index, double r1_in, struct real_clist *r_clst);
void set_from_real_clist_at_index(int index, struct real_clist *r_clst, double *r1_out);

void add_real_clist_before_c_tbl(double ref, double r1_in, struct real_clist *r_clst);
void add_real_clist_after_c_tbl(double ref, double r1_in, struct real_clist *r_clst);
void del_real_clist_by_c_tbl(double ref, struct real_clist *r_clst);
void update_real_clist_by_c_tbl(double ref, double r1_in, struct real_clist *r_clst);
void set_from_real_clist_at_c_tbl(double ref, struct real_clist *r_clst, double *r1_out);

void copy_from_real_clist(struct real_clist *r_clst, int num, double *v1);
void copy_to_real_clist(int num, double *v1, struct real_clist *r_clst);


#endif /* t_control_real_IO_h_ */
