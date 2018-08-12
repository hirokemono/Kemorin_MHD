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
#include "kemosrc_param_c.h"
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


/* prototypes */

void init_real2_ctl_item_c(struct real2_ctl_item *r2_item);
int read_real2_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                          struct real2_ctl_item *r2_item);
int write_real2_ctl_item_c(FILE *fp, int level, int maxlen, 
			const char *label, struct real2_ctl_item *r2_item);

void init_real2_ctl_list(struct real2_ctl_list *head);
void clear_real2_ctl_list(struct real2_ctl_list *head);
struct real2_ctl_list *add_real2_ctl_list(struct real2_ctl_list *current);
void delete_real2_ctl_list(struct real2_ctl_list *current);
int count_real2_ctl_list(struct real2_ctl_list *head);
struct real2_ctl_list *set_real2_ctl_list_pointer(int index, struct real2_ctl_list *head);

int read_real2_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct real2_ctl_list *head);
int write_real2_ctl_list(FILE *fp, int level, const char *label, 
                       struct real2_ctl_list *head);

#endif /* t_control_real2_IO_h_ */
