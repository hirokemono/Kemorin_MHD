
/* control_elements_IO_c.h */

#ifndef CONTROL_ELEMENT_IO_C_
#define CONTROL_ELEMENT_IO_C_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calypso_param_c.h"
#include "skip_comment_c.h"

struct maxlen_2{
    int mlen[2];
};
struct maxlen_3{
    int mlen[3];
};

struct label_list_f{
	int maxlen;
	int num_labels;
	char **label;
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

int write_array_flag_for_ctl_c(FILE *fp, int level, const char *label);
int write_end_array_flag_for_ctl_c(FILE *fp, int level, const char *label);

void skip_comment_read_line(FILE *fp, char buf[LENGTHBUF]);

int right_file_flag_c(const char buf[LENGTHBUF], const char *label);
int read_file_flag_c(const char buf[LENGTHBUF], char *file_name);

int right_begin_flag_c(const char buf[LENGTHBUF], const char *label);
int find_control_end_flag_c(const char buf[LENGTHBUF], const char *label);
int find_control_array_flag_c(const char buf[LENGTHBUF], const char *label);
int find_control_end_array_flag_c(const char buf[LENGTHBUF], const char *label);

int count_max_length_of_label(int num, const char *label[KCHARA_C]);

int find_direction_from_ctl(const char *c_tbl);
void set_direction_from_ctl(int i, char *c_tbl);

struct label_list_f * alloc_ctl_label(void);
void dealloc_ctl_label(struct label_list_f *label_list);
void set_labels_from_packed(int len_fix, char *packed_name, struct label_list_f *label_list);

#endif
