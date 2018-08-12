/*
//  control_lists_IO_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#ifndef control_lists_IO_c_h_
#define control_lists_IO_c_h_


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"
#include "control_3elements_IO_c.h"


struct chara3_ctl_list{
    struct chara3_ctl_item *c3_item;
    struct maxlen_3 *mlen3;
    
    struct chara3_ctl_list *_prev;
    struct chara3_ctl_list *_next;
};

struct real3_ctl_list{
    struct real3_ctl_item *r3_item;
    struct maxlen_1 *mlen1;
    
    struct real3_ctl_list *_prev;
    struct real3_ctl_list *_next;
};

struct chara2_real_ctl_list{
    struct chara2_real_ctl_item *c2r_item;
    struct maxlen_3 *mlen3;
    
    struct chara2_real_ctl_list *_prev;
    struct chara2_real_ctl_list *_next;
};

/* prototypes */

void init_chara3_ctl_list(struct chara3_ctl_list *head);
void clear_chara3_ctl_list(struct chara3_ctl_list *head);
struct chara3_ctl_list *add_chara3_ctl_list(struct chara3_ctl_list *current);
void delete_chara3_ctl_list(struct chara3_ctl_list *current);
int count_chara3_ctl_list(struct chara3_ctl_list *head);
struct chara3_ctl_list *set_chara3_ctl_list_pointer(int index, struct chara3_ctl_list *head);

int read_chara3_ctl_list(FILE *fp, char buf[LENGTHBUF], const char *label, 
                      struct chara3_ctl_list *head);
int write_chara3_ctl_list(FILE *fp, int level, const char *label, 
                       struct chara3_ctl_list *head);


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


#endif /* control_lists_IO_c_h_ */
