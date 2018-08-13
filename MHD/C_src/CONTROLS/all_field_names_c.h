
/*  all_field_names_c.h */

#ifndef ALL_FIELD_NAMES_C_
#define ALL_FIELD_NAMES_C_

#include <stdlib.h>
#include "kemosrc_param_c.h"

#define NUM_FIELD 255
#define NCHARA_FIELD 30

struct field_def{
	int num_comps;
	char field_name[NCHARA_FIELD];
	char field_math[KCHARA_C];
};

struct all_field_ctl_c{
	
	char field_name[NCHARA_FIELD];
	char field_math[KCHARA_C];
	int num_comp;
	int iflag_use;
	int iflag_viz;
	int iflag_monitor;
};

/* Prototypes */ 

int get_field_properties(int index, char *name, char *math);

#endif
