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
#include "kemosrc_param_c.h"
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


/* prototypes */

void init_int2_ctl_item_c(struct int2_ctl_item *i2_item);
int read_int2_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                          struct int2_ctl_item *i2_item);
int write_int2_ctl_item_c(FILE *fp, int level, int maxlen, 
			const char *label, struct int2_ctl_item *i2_item);

void init_int2_ctl_list(struct int2_ctl_list *head);
void clear_int2_ctl_list(struct int2_ctl_list *head);
struct int2_ctl_list *add_int2_ctl_list(struct int2_ctl_list *current);
void delete_int2_ctl_list(struct int2_ctl_list *current);
int count_int2_ctl_list(struct int2_ctl_list *head);
struct int2_ctl_list *set_int2_ctl_list_pointer(int index, struct int2_ctl_list *head);

int read_int2_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct int2_ctl_list *head);
int write_int2_ctl_list(FILE *fp, int level, const char *label, 
                       struct int2_ctl_list *head);

#endif /* t_control_int2_IO_h_ */
