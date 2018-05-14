
/* control_elements_IO_c.h */

#ifndef CONTROL_ELEMENT_IO_C_
#define CONTROL_ELEMENT_IO_C_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "kemosrc_param_c.h"
#include "skip_comment_c.h"

/* Prototypes */ 

void write_space_4_parse_c(FILE *FP, int level);
void write_one_label_cont_c(FILE *FP, char *label);

void write_begin_flag_for_ctl_c(FILE *FP, int level, const char *label);
void write_end_flag_for_ctl_c(FILE *FP, int level, char *label);

void write_array_flag_for_ctl_c(FILE *fp, int level, char *label, int num);
void write_end_array_flag_for_ctl_c(FILE *fp, int level, char *label);

int right_file_flag_c(FILE *fp, const char *label);


#endif
