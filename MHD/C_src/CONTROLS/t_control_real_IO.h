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
#include "kemosrc_param_c.h"
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


/* prototypes */

void init_real_ctl_item_c(struct real_ctl_item *r_item);
int read_real_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                          struct real_ctl_item *r_item);
int write_real_ctl_item_c(FILE *fp, int level, int maxlen, 
			const char *label, struct real_ctl_item *r_item);

void init_real_ctl_list(struct real_ctl_list *head);
void clear_real_ctl_list(struct real_ctl_list *head);
struct real_ctl_list *add_real_ctl_list(struct real_ctl_list *current);
void delete_real_ctl_list(struct real_ctl_list *current);
int count_real_ctl_list(struct real_ctl_list *head);
struct real_ctl_list *set_real_ctl_list_pointer(int index, struct real_ctl_list *head);

int read_real_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct real_ctl_list *head);
int write_real_ctl_list(FILE *fp, int level, const char *label, 
                       struct real_ctl_list *head);

#endif /* t_control_real_IO_h_ */
