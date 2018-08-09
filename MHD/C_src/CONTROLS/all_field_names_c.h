
/*  all_field_names_c.h */

#ifndef ALL_FIELD_NAMES_C_
#define ALL_FIELD_NAMES_C_

#include <stdlib.h>

#define NUM_FIELD 250
#define NCHARA_FIELD 30

struct all_field_def{
	int num_field;
	char **field_names;
	int *field_comps;
};

/* Prototypes */ 

int alloc_copy_field_cond_dimension_list_c(struct all_field_def *fld_def);
void dealloc_copy_field_cond_dimension_list_c(struct all_field_def *fld_def);


#endif