/*
//  t_control_chara2_real_IO.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#ifndef t_control_chara2_real_IO_h_
#define t_control_chara2_real_IO_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"


struct chara2_real_ctl_item{
	int iflag;
	char *c1_tbl;
	char *c2_tbl;
	double r_data;
};

struct chara2_real_ctl_list{
    struct chara2_real_ctl_item *c2r_item;
    struct maxlen_3 *mlen3;
    
    struct chara2_real_ctl_list *_prev;
    struct chara2_real_ctl_list *_next;
};

/* prototypes */

void alloc_c2r_ctl_item_c(struct chara2_real_ctl_item *c2r_item);
void dealloc_c2r_ctl_item_c(struct chara2_real_ctl_item *c2r_item);
int read_c2r_ctl_item_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct chara2_real_ctl_item *c2r_item);
int write_c2r_ctl_item_c(FILE *fp, int level, int maxlen[3], 
			const char *label, struct chara2_real_ctl_item *c2r_item);


void init_c2r_ctl_list(struct chara2_real_ctl_list *head);
void clear_c2r_ctl_list(struct chara2_real_ctl_list *head);
struct chara2_real_ctl_list *add_c2r_ctl_list(struct chara2_real_ctl_list *current);
void delete_c2r_ctl_list(struct chara2_real_ctl_list *current);
int count_c2r_ctl_list(struct chara2_real_ctl_list *head);
struct chara2_real_ctl_list *set_c2r_ctl_list_pointer(int index, struct chara2_real_ctl_list *head);

int read_c2r_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara2_real_ctl_list *head);
int write_c2r_ctl_list(FILE *fp, int level, const char *label, 
                       struct chara2_real_ctl_list *head);

#endif /* t_control_chara2_real_IO_h_ */
