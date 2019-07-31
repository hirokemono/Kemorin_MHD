
/*  all_field_names_c.h */

#ifndef ALL_FIELD_NAMES_C_
#define ALL_FIELD_NAMES_C_

#include <stdlib.h>
#include "kemosrc_param_c.h"

#define NUM_FIELD 263
#define NCHARA_FIELD 30

struct field_def{
	int num_comps;
	char field_name[NCHARA_FIELD];
	char field_math[KCHARA_C];
};

/* Prototypes */ 

int get_field_properties(int index, char *name, char *math);

#endif
