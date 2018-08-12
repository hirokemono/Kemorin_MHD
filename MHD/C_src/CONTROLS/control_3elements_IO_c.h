/*
//  control_3elements_IO_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/08/11.
*/

#ifndef control_3elements_IO_c_h_
#define control_3elements_IO_c_h_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kemosrc_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"

struct maxlen_1{
    int mlen;
};
struct maxlen_3{
    int mlen[3];
};

struct chara3_ctl_item{
	
	int iflag;
	char *c1_tbl;
	char *c2_tbl;
	char *c3_tbl;
};

struct real3_ctl_item{
	
	int iflag;
	double r_data[3];
};

struct chara2_real_ctl_item{
	
	int iflag;
	char *c1_tbl;
	char *c2_tbl;
	double r_data;
};

/* prototypes */

void alloc_chara3_ctl_item_c(struct chara3_ctl_item *c3_item);
void dealloc_chara3_ctl_item_c(struct chara3_ctl_item *c3_item);
int read_chara3_ctl_item_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct chara3_ctl_item *c3_item);
int write_chara3_ctl_item_c(FILE *fp, int level, int maxlen[3], 
			const char *label, struct chara3_ctl_item *c3_item);

void init_real3_ctl_item_c(struct real3_ctl_item *r3_item);
int read_real3_ctl_item_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct real3_ctl_item *r3_item);
int write_real3_ctl_item_c(FILE *fp, int level, int maxlen, 
			const char *label, struct real3_ctl_item *r3_item);

void alloc_c2r_ctl_item_c(struct chara2_real_ctl_item *c2r_item);
void dealloc_c2r_ctl_item_c(struct chara2_real_ctl_item *c2r_item);
int read_c2r_ctl_item_c(FILE *fp, char buf[LENGTHBUF], 
			const char *label, struct chara2_real_ctl_item *c2r_item);
int write_c2r_ctl_item_c(FILE *fp, int level, int maxlen[3], 
			const char *label, struct chara2_real_ctl_item *c2r_item);

#endif /* control_3elements_IO_c_h_ */
