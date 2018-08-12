/*
//  t_control_chara_IO.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#ifndef t_control_chara_IO_h_
#define t_control_chara_IO_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"


struct chara_ctl_item{
	int iflag;
	char *c_tbl;
};

struct chara_ctl_list{
    struct chara_ctl_item *c_item;
    
    struct chara_ctl_list *_prev;
    struct chara_ctl_list *_next;
};


/* prototypes */

void alloc_chara_ctl_item_c(struct chara_ctl_item *c_item);
void dealloc_chara_ctl_item_c(struct chara_ctl_item *c_item);
int read_chara_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                          struct chara_ctl_item *c_item);
int write_chara_ctl_item_c(FILE *fp, int level, int maxlen, 
			const char *label, struct chara_ctl_item *c_item);

int find_boolean_from_chara_ctl_item(struct chara_ctl_item *c_item);
void set_boolean_by_chara_ctl_item(int iflag, struct chara_ctl_item *c_item);

void init_chara_ctl_list(struct chara_ctl_list *head);
void clear_chara_ctl_list(struct chara_ctl_list *head);
struct chara_ctl_list *add_chara_ctl_list(struct chara_ctl_list *current);
void delete_chara_ctl_list(struct chara_ctl_list *current);
int count_chara_ctl_list(struct chara_ctl_list *head);
struct chara_ctl_list *set_chara_ctl_list_pointer(int index, struct chara_ctl_list *head);

int read_chara_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara_ctl_list *head);
int write_chara_ctl_list(FILE *fp, int level, const char *label, 
                       struct chara_ctl_list *head);

#endif /* t_control_chara_IO_h_ */
