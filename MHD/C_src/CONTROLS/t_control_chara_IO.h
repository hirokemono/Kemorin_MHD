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
#include "calypso_param_c.h"
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

struct chara_clist{
	struct chara_ctl_list c_item_head;
    
    int iflag_use;
    char *clist_name;
    char *c1_name;
};


/* prototypes */

struct chara_ctl_item * init_chara_ctl_item_c(void);
void dealloc_chara_ctl_item_c(struct chara_ctl_item *c_item);
int read_chara_ctl_item_c(char buf[LENGTHBUF], const char *label, 
                          struct chara_ctl_item *c_item);
int write_chara_ctl_item_c(FILE *fp, int level, int maxlen, 
			const char *label, struct chara_ctl_item *c_item);

void update_chara_ctl_item_c(char *c_in, struct chara_ctl_item *c_item);
void set_from_chara_ctl_item_c( struct chara_ctl_item *c_item, char *c_out);

int find_boolean_from_chara_ctl_item(struct chara_ctl_item *c_item);
void set_boolean_by_chara_ctl_item(int iflag, struct chara_ctl_item *c_item);

void copy_from_chara_ctl_item(struct chara_ctl_item *c_item, char *c_data);
void copy_to_chara_ctl_item(const char *c_data, struct chara_ctl_item *c_item);

void init_chara_ctl_list(struct chara_ctl_list *head);
void clear_chara_ctl_list(struct chara_ctl_list *head);
struct chara_ctl_list *add_chara_ctl_list_after(struct chara_ctl_list *current);
void delete_chara_ctl_list(struct chara_ctl_list *current);
int count_chara_ctl_list(struct chara_ctl_list *head);
struct chara_ctl_list *find_c_ctl_list_item_by_index(int index, struct chara_ctl_list *head);
struct chara_ctl_list *find_c_ctl_list_item_by_c_tbl(char *ref, struct chara_ctl_list *head);

int read_chara_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara_ctl_list *head);
int write_chara_ctl_list(FILE *fp, int level, const char *label, 
                       struct chara_ctl_list *head);


void append_chara_ctl_list(char *c_in, struct chara_ctl_list *head);

void del_chara_ctl_list_by_c_tbl(char *ref, struct chara_ctl_list *head);


struct chara_clist * init_chara_clist(void);
void dealloc_chara_clist(struct chara_clist *c_clst);
int count_chara_clist(struct chara_clist *c_clst);
void read_chara_clist(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara_clist *c_clst);
int write_chara_clist(FILE *fp, int level, const char *label, 
                       struct chara_clist *c_clst);

void append_chara_clist(char *c_in, struct chara_clist *c_clst);
void del_chara_clist_by_index(int index, struct chara_clist *c_clst);
void update_chara_clist_by_index(int index, char *c_in, struct chara_clist *c_clst);
void set_from_chara_clist_at_index(int index, struct chara_clist *c_clst, char *c_out);
struct chara_ctl_item *chara_clist_at_index(int index, struct chara_clist *c_clst);

void add_chara_clist_before_c_tbl(char *ref, char *c_in, struct chara_clist *c_clst);
void add_chara_clist_after_c_tbl(char *ref, char *c_in, struct chara_clist *c_clst);
void del_chara_clist_by_c_tbl(char *ref, struct chara_clist *c_clst);
void update_chara_clist_by_c_tbl(char *ref, char *c_in, struct chara_clist *c_clst);
void set_from_chara_clist_at_c_tbl(char *ref, struct chara_clist *c_clst, char *c_out);

#endif /* t_control_chara_IO_h_ */
