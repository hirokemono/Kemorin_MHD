/*
//  control_arrays_IO_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/05/24.
*/

#ifndef control_arrays_IO_c_h__
#define control_arrays_IO_c_h__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "kemosrc_param_c.h"
#include "skip_comment_c.h"
#include "control_elements_IO_c.h"

struct chara_ctl_array{
    int maxlen;
    
    int num;
    int icou;
    struct chara_ctl_item **c_array_item;
};

struct int_ctl_array{
    int maxlen;
    
    int num;
    int icou;
    struct int_ctl_item **i_array_item;
};

struct i2_ctl_array{
    int maxlen;
    
    int num;
    int icou;
    struct int_ctl_item **i1_array_item;
    struct int_ctl_item **i2_array_item;
};

struct chara_real_ctl_array{
    int maxlen[2];
    
    int num;
    int icou;
    struct chara_ctl_item **c_array_item;
    struct real_ctl_item **r_array_item;
};

struct chara3_ctl_array{
    int maxlen[3];
    
    int num;
    int icou;
    struct chara_ctl_item **c1_array_item;
    struct chara_ctl_item **c2_array_item;
    struct chara_ctl_item **c3_array_item;
};

struct chara2_real_ctl_array{
    int maxlen[3];
    
    int num;
    int icou;
    struct chara_ctl_item **c1_array_item;
    struct chara_ctl_item **c2_array_item;
    struct real_ctl_item **r_array_item;
};

/* prototype */


void alloc_ctl_chara_array(struct chara_ctl_array *c_array);
void dealloc_ctl_chara_array(struct chara_ctl_array *c_array);
void read_character_ctl_array_c(FILE *fp, char *buf, const char *label,
			struct chara_ctl_array *c_array);
void write_character_ctl_array_c(FILE *fp, int level, int maxlen,
			const char *label, struct chara_ctl_array *c_array);

void alloc_ctl_int_array(struct int_ctl_array *i_array);
void dealloc_ctl_int_array(struct int_ctl_array *i_array);
void read_integer_ctl_array_c(FILE *fp, char *buf, const char *label,
			struct int_ctl_array *i_array);
void write_integer_ctl_array_c(FILE *fp, int level, int maxlen,
			const char *label, struct int_ctl_array *i_array);

void alloc_ctl_i2_array(struct i2_ctl_array *i2_array);
void dealloc_ctl_i2_array(struct i2_ctl_array *i2_array);
void read_int2_ctl_array_c(FILE *fp, char *buf, const char *label,
			struct i2_ctl_array *i2_array);
void write_int2_ctl_array_c(FILE *fp, int level, int maxlen,
			const char *label, struct i2_ctl_array *i2_array);

void alloc_ctl_cr_array(struct chara_real_ctl_array *cr_array);
void dealloc_ctl_cr_array(struct chara_real_ctl_array *cr_array);
void read_cr_ctl_array_c(FILE *fp, char *buf, const char *label,
			struct chara_real_ctl_array *cr_array);
void write_cr_ctl_array_c(FILE *fp, int level, int maxlen,
			const char *label, struct chara_real_ctl_array *cr_array);

void alloc_ctl_chara3_array(struct chara3_ctl_array *c3_array);
void dealloc_ctl_chara3_array(struct chara3_ctl_array *c3_array);
void read_chara3_ctl_array_c(FILE *fp, char *buf, const char *label,
			struct chara3_ctl_array *c3_array);
void write_chara3_ctl_array_c(FILE *fp, int level, int maxlen,
			const char *label, struct chara3_ctl_array *c3_array);

void alloc_ctl_c2r_array(struct chara2_real_ctl_array *c2r_array);
void dealloc_ctl_c2r_array(struct chara2_real_ctl_array *c2r_array);
void read_c2r_ctl_array_c(FILE *fp, char *buf, const char *label,
			struct chara2_real_ctl_array *c2r_array);
void write_c2r_ctl_array_c(FILE *fp, int level, int maxlen,
			const char *label, struct chara2_real_ctl_array *c2r_array);


#endif /* control_arrays_IO_c.h */
