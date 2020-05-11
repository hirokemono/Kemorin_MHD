
/* skip_comment_c.h */

#ifndef SKIP_COMMENT_C_
#define SKIP_COMMENT_C_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "calypso_param_c.h"

/* Prototypes */ 
char* alloc_string(long lengthchara);

char *ltrim(char *s);
char *trim(char *s);
char *trim(char *s);

long skip_comment_c(FILE *fp);

int set_field_coordinate_flag(const char *data_name);
int read_field_name_from_buffer(int len_buf, char *buf, char *data_name);
void read_field_names(FILE *fp, int num, char **data_name, int *id_coord);
int count_comps_by_comma_c(FILE *fp);
void read_multi_field_name(FILE *fp, char **data_name);

void strngcopy(char *chara_out, const char *chara_in);
int compare_string(int length, const char *string1, const char *string2);
int cmp_no_case_c(const char *string1, const char *string2);
int check_cautation_require(const char *string);
void strip_cautation_marks(char *string);
char * duplicate_underscore(const char *string);


int get_index_from_file_head(const char *file_head, char *stripped_fhead);
void get_ext_from_file_name_c(const char *file_name,
							  char *stripped_fhead, char *stripped_ext);

void split_dir_and_file_name_c(const char *file_name,
                               char *stripped_dir, char *stripped_fname);

void add_ext_to_file_name_c(const char *file_head,
							 const char *added_ext, char *file_name);

int toggle_value_c(int current);

#endif
