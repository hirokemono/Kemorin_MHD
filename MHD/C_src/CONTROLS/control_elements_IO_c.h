
/* control_elements_IO_c.h */

#ifndef CONTROL_ELEMENT_IO_C_
#define CONTROL_ELEMENT_IO_C_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "kemosrc_param_c.h"
#include "skip_comment_c.h"

struct maxlen_1{
    int mlen;
};
struct maxlen_2{
    int mlen[2];
};
struct maxlen_3{
    int mlen[3];
};

struct chara_ctl_item{
	int iflag;
	char *c_tbl;
};
struct int_ctl_item{
	int iflag;
	int i_data;
};
struct real_ctl_item{
	int iflag;
	double r_data;
};


/* Prototypes */ 

void write_spaces_c(FILE *fp, int num_space);
void write_space_4_parse_c(FILE *FP, int level);
void adjust_text_output(FILE *fp, int maxlen,  const char *label);
void write_one_label_cont_c(FILE *fp, int maxlen, const char *label);
void write_one_label_w_lf_c(FILE *fp, const char *label);

void write_file_flag_for_ctl_c(FILE *fp, int level, const char *label, const char *file_name);
int write_begin_flag_for_ctl_c(FILE *FP, int level, const char *label);
int write_end_flag_for_ctl_c(FILE *FP, int level, const char *label);

int write_array_flag_for_ctl_c(FILE *fp, int level, const char *label, int num);
int write_end_array_flag_for_ctl_c(FILE *fp, int level, const char *label);

void skip_comment_read_line(FILE *fp, char buf[LENGTHBUF]);

int right_file_flag_c(const char buf[LENGTHBUF], const char *label);
int read_file_flag_c(const char buf[LENGTHBUF], char *file_name);

int right_begin_flag_c(const char buf[LENGTHBUF], const char *label);
int find_control_end_flag_c(const char buf[LENGTHBUF], const char *label);
int find_control_array_flag_c(const char buf[LENGTHBUF], const char *label, int *num);
int find_control_end_array_flag_c(const char buf[LENGTHBUF], const char *label, int num, int icou);

int count_max_length_of_label(int num, const char *label[KCHARA_C]);

void alloc_ctl_chara_item(struct chara_ctl_item *c_item);
void dealloc_ctl_chara_item(struct chara_ctl_item *c_item);
void read_character_ctl_item_c(const char *buf, const char *label,
			struct chara_ctl_item *c_item);
void write_character_ctl_item_c(FILE *fp, int level, int maxlen,
			const char *label, struct chara_ctl_item *c_item);
int find_boolean_from_chara_ctl_item(struct chara_ctl_item *c_item);
void set_boolean_by_chara_ctl_item(int iflag, struct chara_ctl_item *c_item);

void init_ctl_int_item(struct int_ctl_item *i_item);
void read_integer_ctl_item_c(const char *buf, const char *label,
			struct int_ctl_item *i_item);
void write_integer_ctl_item_c(FILE *fp, int level, int maxlen,
			const char *label, struct int_ctl_item *i_item);

void init_ctl_real_item(struct real_ctl_item *r_item);
void read_real_ctl_item_c(const char *buf, const char *label,
			struct real_ctl_item *r_item);
void write_real_ctl_item_c(FILE *fp, int level, int maxlen,
			const char *label, struct real_ctl_item *r_item);


void read_int2_ctl_item_c(const char *buf, const char *label,
			struct int_ctl_item *i1_item, struct int_ctl_item *i2_item);
void write_int2_ctl_item_c(FILE *fp, int level, int maxlen, const char *label,
			struct int_ctl_item *i1_item, struct int_ctl_item *i2_item);

void read_real2_ctl_item_c(const char *buf, const char *label,
			struct real_ctl_item *r1_item, struct real_ctl_item *r2_item);
void write_real2_ctl_item_c(FILE *fp, int level, int maxlen, const char *label,
			struct real_ctl_item *r1_item, struct real_ctl_item *r2_item);

void read_ci_ctl_item_c(const char *buf, const char *label,
			struct chara_ctl_item *c_item, struct int_ctl_item *i_item);
void write_ci_ctl_item_c(FILE *fp, int level, int maxlen[2], const char *label, 
			struct chara_ctl_item *c_item, struct int_ctl_item *i_item);

void read_cr_ctl_item_c(const char *buf, const char *label,
			struct chara_ctl_item *c_item, struct real_ctl_item *r_item);
void write_cr_ctl_item_c(FILE *fp, int level, int maxlen[2], const char *label, 
			struct chara_ctl_item *c_item, struct real_ctl_item *r_item);

void read_ir_ctl_item_c(const char *buf, const char *label,
			struct int_ctl_item *i_item, struct real_ctl_item *r_item);
void write_ir_ctl_item_c(FILE *fp, int level, int maxlen, const char *label, 
			struct int_ctl_item *i_item, struct real_ctl_item *r_item);

#endif
