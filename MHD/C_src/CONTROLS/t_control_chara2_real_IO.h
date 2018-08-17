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
    
    struct chara2_real_ctl_list *_prev;
    struct chara2_real_ctl_list *_next;
};

struct chara2_real_clist{
	struct chara2_real_ctl_list c2r_item_head;
};

/* prototypes */

void alloc_c2r_ctl_item_c(struct chara2_real_ctl_item *c2r_item);
void dealloc_c2r_ctl_item_c(struct chara2_real_ctl_item *c2r_item);
int read_c2r_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                        struct chara2_real_ctl_item *c2r_item);
int write_c2r_ctl_item_c(FILE *fp, int level, int maxlen[3], 
			const char *label, struct chara2_real_ctl_item *c2r_item);

void update_chara2_real_ctl_item_c(char *c1_in, char *c2_in, double r_in,  
                              struct chara2_real_ctl_item *c2r_item);
void set_from_chara2_real_ctl_item_c(struct chara2_real_ctl_item *c2r_item,
                              char *c1_out, char *c2_out, double *r_out);

struct chara2_real_ctl_list *add_c2r_ctl_list(struct chara2_real_ctl_list *current);
void delete_c2r_ctl_list(struct chara2_real_ctl_list *current);
int count_c2r_ctl_list(struct chara2_real_ctl_list *head);
struct chara2_real_ctl_list *find_c2r_ctl_list_item_by_index(int index,
			struct chara2_real_ctl_list *head);
struct chara2_real_ctl_list *find_c2r_ctl_list_item_by_c_tbl(char *ref,
			struct chara2_real_ctl_list *head);

int read_c2r_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara2_real_ctl_list *head);
int write_c2r_ctl_list(FILE *fp, int level, const char *label, 
                       struct chara2_real_ctl_list *head);

void append_chara2_real_ctl_list(char *c1_in, char *c2_in, double r_in,
                      struct chara2_real_ctl_list *head);
void del_chara2_real_ctl_list_by_index(int index, struct chara2_real_ctl_list *head);
void update_chara2_real_ctl_list_by_index(int index, char *c1_in, char *c2_in, double r_in,
			struct chara2_real_ctl_list *head);
void set_from_chara2_real_ctl_list_at_index(int index, struct chara2_real_ctl_list *head,
			char *c1_out, char *c2_out, double *r_out);

void del_chara2_real_ctl_list_by_c_tbl(char *ref, struct chara2_real_ctl_list *head);
void update_chara2_real_ctl_list_by_c_tbl(char *ref, char *c1_in, char *c2_in, double r_in,
			struct chara2_real_ctl_list *head);


void init_c2r_clist(struct chara2_real_clist *c2r_clst);
void clear_c2r_clist(struct chara2_real_clist *c2r_clst);
int count_c2r_clist(struct chara2_real_clist *c2r_clst);

int read_c2r_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara2_real_clist *c2r_clst);
int write_c2r_clist(FILE *fp, int level, const char *label, 
                       struct chara2_real_clist *c2r_clst);

void append_chara2_real_clist(char *c1_in, char *c2_in, double r_in,
                      struct chara2_real_clist *c2r_clst);
void del_chara2_real_clist_by_index(int index, struct chara2_real_clist *c2r_clst);
void update_chara2_real_clist_by_index(int index, char *c1_in, char *c2_in, double r_in,
			struct chara2_real_clist *c2r_clst);
void set_from_chara2_real_clist_at_index(int index, struct chara2_real_clist *c2r_clst,
			char *c1_out, char *c2_out, double *r_out);

void del_chara2_real_clist_by_c_tbl(char *ref, struct chara2_real_clist *c2r_clst);
void update_chara2_real_clist_by_c_tbl(char *ref, char *c1_in, char *c2_in, double r_in,
			struct chara2_real_clist *c2r_clst);
void set_from_chara2_real_clist_at_c_tbl(char *ref, struct chara2_real_clist *c2r_clst,
			char *c1_out, char *c2_out, double *r_out);

#endif /* t_control_chara2_real_IO_h_ */
