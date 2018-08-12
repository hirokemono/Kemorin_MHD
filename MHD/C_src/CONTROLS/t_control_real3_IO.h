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
#include "kemosrc_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"


struct real3_ctl_item{
	
	int iflag;
	double r_data[3];
};

struct real3_ctl_list{
    struct real3_ctl_item *r3_item;
    
    struct real3_ctl_list *_prev;
    struct real3_ctl_list *_next;
};

/* prototypes */


void init_real3_ctl_item_c(struct real3_ctl_item *r3_item);
int read_real3_ctl_item_c(char buf[LENGTHBUF], const char *label, 
			struct real3_ctl_item *r3_item);
int write_real3_ctl_item_c(FILE *fp, int level, int maxlen, 
			const char *label, struct real3_ctl_item *r3_item);


void init_real3_ctl_list(struct real3_ctl_list *head);
void clear_real3_ctl_list(struct real3_ctl_list *head);
struct real3_ctl_list *add_real3_ctl_list(struct real3_ctl_list *current);
void delete_real3_ctl_list(struct real3_ctl_list *current);
int count_real3_ctl_list(struct real3_ctl_list *head);
struct real3_ctl_list *set_real3_ctl_list_pointer(int index, struct real3_ctl_list *head);

int read_real3_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct real3_ctl_list *head);
int write_real3_ctl_list(FILE *fp, int level, const char *label, 
                       struct real3_ctl_list *head);


void copy_from_real3_ctl_item(struct real3_ctl_item *r3_item, 
			double *r1, double *r2, double *r3);
void copy_to_real3_ctl_item(double r1, double r2, double r3,
			struct real3_ctl_item *r3_item);

void copy_from_real3_ctl_list(struct real3_ctl_list *head, int num,
			double *v1, double *v2, double *v3);
void copy_to_real3_ctl_list(int num, double *v1, double *v2, double *v3,
			struct real3_ctl_list *head);


#endif /* t_control_real3_IO_h_ */
