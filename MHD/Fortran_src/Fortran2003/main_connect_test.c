#include <stdlib.h>
#include <stdio.h>

#include "all_field_names_c.h"

#define LENGTHBUF 2048

FILE *fp;

extern void c_read_control_sph_SGS_MHD();
extern void c_write_control_sph_SGS_MHD();

struct field_control{
	struct all_field_def *fld_def;
	int *iflag_use;
	int *iflag_viz;
	int *iflag_monitor;
};

struct field_control *fld_ctl;

int main(int argc,char *argv[])
{
	char buf[LENGTHBUF];      /* character buffer for reading line */
	char ctmp[32];            /* character buffer for reading line */
	char ctmp2[32];            /* character buffer for reading line */
	int i, j;
	
	c_read_control_sph_SGS_MHD();
	c_write_control_sph_SGS_MHD();
	
	fld_ctl = (struct field_control *) malloc(sizeof(struct field_control));
	fld_ctl->fld_def = (int *)malloc(sizeof(int));
	fld_ctl->iflag_use = (int *)calloc(fld_ctl->fld_def->num_field, sizeof(int));
	fld_ctl->iflag_viz = (int *)calloc(fld_ctl->fld_def->num_field, sizeof(int));
	fld_ctl->iflag_monitor = (int *)calloc(fld_ctl->fld_def->num_field, sizeof(int));
	
	i = alloc_copy_field_cond_dimension_list_c(fld_ctl->fld_def);
	
	
		printf("baka %d\n", fld_ctl->fld_def->num_field);
	for(i=0;i<fld_ctl->fld_def->num_field;i++){
		printf("field_name %d: %s %d\n", i, fld_ctl->fld_def->field_names[i], fld_ctl->fld_def->field_comps[i]);
	}
	
	return 0;
}
