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

struct int2_ctl_array{
    int maxlen;
    
    int num;
    int icou;
    struct int_ctl_item **i1_array_item;
    struct int_ctl_item **i2_array_item;
};

struct int_real_ctl_array{
    int maxlen;
    
    int num;
    int icou;
    struct int_ctl_item **i_array_item;
    struct real_ctl_item **r_array_item;
};

/* prototype */


void init_ctl_chara_array(struct chara_ctl_array *c_array);
void alloc_ctl_chara_array(struct chara_ctl_array *c_array);
void dealloc_ctl_chara_array(struct chara_ctl_array *c_array);
void read_character_ctl_array_c(FILE *fp, char *buf, const char *label,
			struct chara_ctl_array *c_array);
void write_character_ctl_array_c(FILE *fp, int level, int maxlen,
			const char *label, struct chara_ctl_array *c_array);

void init_ctl_int_array(struct int_ctl_array *i_array);
void alloc_ctl_int_array(struct int_ctl_array *i_array);
void dealloc_ctl_int_array(struct int_ctl_array *i_array);
void read_integer_ctl_array_c(FILE *fp, char *buf, const char *label,
			struct int_ctl_array *i_array);
void write_integer_ctl_array_c(FILE *fp, int level, int maxlen,
			const char *label, struct int_ctl_array *i_array);

void init_ctl_int2_array(struct int2_ctl_array *i2_array);
void alloc_ctl_int2_array(struct int2_ctl_array *i2_array);
void dealloc_ctl_int2_array(struct int2_ctl_array *i2_array);
void read_int2_ctl_array_c(FILE *fp, char *buf, const char *label,
			struct int2_ctl_array *i2_array);
void write_int2_ctl_array_c(FILE *fp, int level, int maxlen,
			const char *label, struct int2_ctl_array *i2_array);

void init_ctl_ir_array(struct int_real_ctl_array *ir_array);
void alloc_ctl_ir_array(struct int_real_ctl_array *ir_array);
void dealloc_ctl_ir_array(struct int_real_ctl_array *ir_array);
void read_ir_ctl_array_c(FILE *fp, char *buf, const char *label,
			struct int_real_ctl_array *ir_array);
void write_ir_ctl_array_c(FILE *fp, int level, int maxlen,
			const char *label, struct int_real_ctl_array *ir_array);

#endif /* control_arrays_IO_c.h */
