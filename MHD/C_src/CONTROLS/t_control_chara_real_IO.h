/*
//  t_control_chara_real_IO.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#ifndef t_control_chara_real_IO_h_
#define t_control_chara_real_IO_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calypso_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"


struct chara_real_ctl_item{
	void * f_self;
	int * f_iflag;
	char * c_block_name;
	
	char *c_tbl;
	double r_data;
};

struct chara_real_ctl_list{
    struct chara_real_ctl_item *cr_item;
    
    struct chara_real_ctl_list *_prev;
    struct chara_real_ctl_list *_next;
};

struct chara_real_clist{
	struct chara_real_ctl_list cr_item_head;

    void *f_self;

    char *clist_name;
    char *c1_name;
    char *r1_name;
    
    int index_bc;
};

/* prototypes */

struct chara_real_ctl_item * init_chara_real_ctl_item_c(void);
void dealloc_chara_real_ctl_item_c(struct chara_real_ctl_item *cr_item);
int read_chara_real_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                               struct chara_real_ctl_item *cr_item);
int write_chara_real_ctl_item_c(FILE *fp, int level, int maxlen[2], 
			const char *label, struct chara_real_ctl_item *cr_item);

void update_chara_real_ctl_item_c(char *c_in, double r_in,  
                              struct chara_real_ctl_item *cr_item);
void set_from_chara_real_ctl_item_c(struct chara_real_ctl_item *cr_item,
                              char *c_out, double *r_out);

void append_chara_real_ctl_list(char *c_in, double r_in,
			struct chara_real_ctl_list *head);
void del_chara_real_ctl_list_by_c_tbl(char *ref, struct chara_real_ctl_list *head);


struct chara_real_clist * init_chara_real_clist(void);
void dealloc_chara_real_clist(struct chara_real_clist *cr_clst);
int count_chara_real_clist(struct chara_real_clist *cr_clst);

int read_chara_real_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara_real_clist *cr_clst);
int write_chara_real_clist(FILE *fp, int level, const char *label, 
                       struct chara_real_clist *cr_clst);

void append_chara_real_clist(char *c_in, double r_in,
                      struct chara_real_clist *cr_clst);
void del_chara_real_clist_by_index(int index, struct chara_real_clist *cr_clst);
void update_chara_real_clist_by_index(int index, char *c_in, double r_in,
			struct chara_real_clist *cr_clst);
void set_from_chara_real_clist_at_index(int index, struct chara_real_clist *cr_clst,
			char *c_out, double *r_out);
struct chara_real_ctl_item *chara_real_clist_at_index(int index, struct chara_real_clist *cr_clst);

void add_chara_real_clist_before_c_tbl(char *ref, char *c_in, double r_in, struct chara_real_clist *cr_clst);
void add_chara_real_clist_after_c_tbl(char *ref, char *c_in, double r_in, struct chara_real_clist *cr_clst);
void del_chara_real_clist_by_c_tbl(char *ref, struct chara_real_clist *cr_clst);
void update_chara_real_clist_by_c_tbl(char *ref, char *c_in, double r_in,
			struct chara_real_clist *cr_clst);
void set_from_chara_real_clist_at_c_tbl(char *ref, struct chara_real_clist *cr_clst,
			char *c_out, double *r_out);

#endif /* t_control_chara_real_IO_h_ */
