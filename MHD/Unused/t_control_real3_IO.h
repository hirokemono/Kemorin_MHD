/*
//  t_control_real3_IO.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#ifndef t_control_real3_IO_h_
#define t_control_real3_IO_h_


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"


struct real3_ctl_item{
	void * f_self;
	int * f_iflag;
	char * c_block_name;
	
	
	int iflag;
	double r_data[3];
};

struct real3_ctl_list{
    struct real3_ctl_item *r3_item;
    
    struct real3_ctl_list *_prev;
    struct real3_ctl_list *_next;
};

struct real3_clist{
	struct real3_ctl_list r3_item_head;

    void *f_self;

    char *clist_name;
    char *r1_name;
    char *r2_name;
    char *r3_name;

    int index_bc;
};

/* prototypes */


struct real3_ctl_item * init_real3_ctl_item_c(void);
void dealloc_real3_ctl_item(struct real3_ctl_item *r3_item);

int read_real3_ctl_item_c(char buf[LENGTHBUF], const char *label,
			struct real3_ctl_item *r3_item);
int write_real3_ctl_item_c(FILE *fp, int level, int maxlen, 
			const char *label, struct real3_ctl_item *r3_item);

void update_real3_ctl_item_c(double r1_in, double r2_in, double r3_in,
                              struct real3_ctl_item *r3_item);
void set_from_real3_ctl_item_c(struct real3_ctl_item *r3_item,
                              double *r1_out, double *r2_out, double *r3_out);



void copy_from_real3_ctl_item(struct real3_ctl_item *r3_item, 
			double *r1, double *r2, double *r3);
void copy_to_real3_ctl_item(double r1, double r2, double r3,
			struct real3_ctl_item *r3_item);

void copy_to_real3_ctl_list(int num, double *v1, double *v2, double *v3,
			struct real3_ctl_list *head);


struct real3_clist * init_real3_clist(void);
void dealloc_real3_clist(struct real3_clist *r3_clst);
int count_real3_clist(struct real3_clist *r3_clst);

int read_real3_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct real3_clist *r3_clst);
int write_real3_clist(FILE *fp, int level, const char *label, 
                       struct real3_clist *r3_clst);

void append_real3_clist(double r1_in, double r2_in, double r3_in, struct real3_clist *r3_clst);
void del_real3_clist_by_index(int index, struct real3_clist *r3_clst);
void update_real3_clist_by_index(int index, double r1_in, double r2_in, double r3_in, 
			struct real3_clist *r3_clst);
void set_from_real3_clist_at_index(int index, struct real3_clist *r3_clst,
			double *r1_out, double *r2_out, double *r3_out);
            
struct real3_ctl_item *real3_clist_at_index(int index, struct real3_clist *r3_clst);

void add_real3_clist_before_c_tbl(double ref_1, double ref_2, double ref_3, 
			struct real3_clist *r3_clst);
void add_real3_clist_after_c_tbl(double ref_1, double ref_2, double ref_3, 
			struct real3_clist *r3_clst);
void del_real3_clist_by_c_tbl(double ref_1, double ref_2, double ref_3,
			struct real3_clist *r3_clst);
void update_real3_clist_by_c_tbl(double ref_1, double ref_2, double ref_3, 
			double r1_in, double r2_in, double r3_in, struct real3_clist *r3_clst);
void set_from_real3_clist_at_c_tbl(double ref_1, double ref_2, double ref_3,
			struct real3_clist *r3_clst, double *r1_out, double *r2_out, double *r3_out);

void copy_from_real3_clist(struct real3_clist *r3_clst, int num,
			double *v1, double *v2, double *v3);
void copy_to_real3_clist(int num, double *v1, double *v2, double *v3,
			struct real3_clist *r3_clst);


#endif /* t_control_real3_IO_h_ */
