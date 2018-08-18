/*
//  t_control_chara_int2_IO.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#ifndef t_control_chara_int2_IO_h_
#define t_control_chara_int2_IO_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"


struct chara_int2_ctl_item{
	int iflag;
	
	char *c_tbl;
	int i_data[2];
};

struct chara_int2_ctl_list{
    struct chara_int2_ctl_item *ci2_item;
    
    struct chara_int2_ctl_list *_prev;
    struct chara_int2_ctl_list *_next;
};

struct chara_int2_clist{
	struct chara_int2_ctl_list ci2_item_head;
};

/* prototypes */

void alloc_chara_int2_ctl_item_c(struct chara_int2_ctl_item *ci2_item);
void dealloc_chara_int2_ctl_item_c(struct chara_int2_ctl_item *ci2_item);
int read_chara_int2_ctl_item_c(char buf[LENGTHBUF], const char *label, struct chara_int2_ctl_item *ci2_item);
int write_chara_int2_ctl_item_c(FILE *fp, int level, int maxlen[2], 
			const char *label, struct chara_int2_ctl_item *ci2_item);

void update_chara_int2_ctl_item_c(char *c_in, int i1_in, int i2_in,  
                              struct chara_int2_ctl_item *ci2_item);
void set_from_chara_int2_ctl_item_c( struct chara_int2_ctl_item *ci2_item,
                              char *c_out, int *i1_out, int *i2_out);

void init_chara_int2_ctl_list(struct chara_int2_ctl_list *head);
void clear_chara_int2_ctl_list(struct chara_int2_ctl_list *head);
struct chara_int2_ctl_list *add_chara_int2_ctl_list_after(struct chara_int2_ctl_list *current);
void delete_chara_int2_ctl_list(struct chara_int2_ctl_list *current);

int count_maxlen_chara_int2_ctl_list(const char *label, 
			struct chara_int2_ctl_list *head, int mlen2[2]);
int count_chara_int2_ctl_list(struct chara_int2_ctl_list *head);
struct chara_int2_ctl_list *find_ci2_ctl_list_item_by_index(int index, struct chara_int2_ctl_list *head);
struct chara_int2_ctl_list *find_ci2_ctl_list_item_by_c_tbl(char *ref, struct chara_int2_ctl_list *head);

int read_chara_int2_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara_int2_ctl_list *head);
int write_chara_int2_ctl_list(FILE *fp, int level, const char *label, 
                       struct chara_int2_ctl_list *head);

void del_chara_int2_ctl_list_by_c_tbl(char *ref, struct chara_int2_ctl_list *head);


void init_chara_int2_clist(struct chara_int2_clist *ci2_clst);
void clear_chara_int2_clist(struct chara_int2_clist *ci2_clst);
int count_chara_int2_clist(struct chara_int2_clist *ci2_clst);

int read_chara_int2_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara_int2_clist *ci2_clst);
int write_chara_int2_clist(FILE *fp, int level, const char *label, 
                       struct chara_int2_clist *ci2_clst);

void append_chara_int2_clist(char *c_in, int i1_in, int i2_in,
                      struct chara_int2_clist *ci2_clst);
void del_chara_int2_clist_by_index(int index, struct chara_int2_clist *ci2_clst);
void update_chara_int2_clist_by_index(int index, char *c_in, int i1_in, int i2_in,
			struct chara_int2_clist *ci2_clst);
void set_from_chara_int2_clist_at_index(int index, struct chara_int2_clist *ci2_clst,
			char *c_out, int *i1_out, int *i2_out);

void del_chara_int2_clist_by_c_tbl(char *ref, struct chara_int2_clist *ci2_clst);
void update_chara_int2_clist_by_c_tbl(char *ref, char *c_in, int i1_in, int i2_in,
			struct chara_int2_clist *ci2_clst);
void set_from_chara_int2_clist_at_c_tbl(char *ref, struct chara_int2_clist *ci2_clst,
			char *c_out, int *i1_out, int *i2_out);

#endif /* t_control_chara_int2_IO_h_ */
