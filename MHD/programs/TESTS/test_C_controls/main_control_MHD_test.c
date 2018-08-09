#include <stdlib.h>
#include <stdio.h>

#include "all_field_names_c.h"
#include "control_elements_IO_c.h"
#include "t_SGS_MHD_control_c.h"

struct field_control{
	struct all_field_def *fld_def;
	int *iflag_use;
	int *iflag_viz;
	int *iflag_monitor;
};

struct field_control *fld_ctl;


int main(int argc,char *argv[])
{
	struct SGS_MHD_control_c *mhd_ctl;
	char buf[LENGTHBUF];      /* character buffer for reading line */
	char file_name[LENGTHBUF] = "/Users/matsui/work/C_test/control_MHD";
	char file_name_2[LENGTHBUF];
	int i;
	
	fld_ctl = (struct field_control *) malloc(sizeof(struct field_control));
	fld_ctl->fld_def = (struct all_field_def*)malloc(sizeof(struct all_field_def));
	fld_ctl->iflag_use = (int *)calloc(fld_ctl->fld_def->num_field, sizeof(int));
	fld_ctl->iflag_viz = (int *)calloc(fld_ctl->fld_def->num_field, sizeof(int));
	fld_ctl->iflag_monitor = (int *)calloc(fld_ctl->fld_def->num_field, sizeof(int));
	
	i = alloc_copy_field_cond_dimension_list_c(fld_ctl->fld_def);
	
	printf("baka %d\n", fld_ctl->fld_def->num_field);
	for(i=0;i<fld_ctl->fld_def->num_field;i++){
		printf("field_name %d: %s %d\n", i, fld_ctl->fld_def->field_names[i], fld_ctl->fld_def->field_comps[i]);
	}
	
	/*
	if (argc <= 1) {
		printf("Input control file in standard input. \n");
		return 1;
	};
	strngcopy(file_name, argv[1]);
    */
	strngcopy(file_name_2, file_name);
	strcat(file_name_2, "_2");
	
	printf("Input file name: %s\n", file_name);
	printf("Copied file name: %s\n", file_name_2);
	
	
	mhd_ctl = (struct SGS_MHD_control_c *) malloc(sizeof(struct SGS_MHD_control_c));
	alloc_SGS_MHD_control_c(mhd_ctl);
	
	read_SGS_MHD_control_file_c(file_name, buf, mhd_ctl);
	rename_SGS_MHD_ctl_subfile_c(mhd_ctl);
	write_SGS_MHD_control_file_c(file_name_2, mhd_ctl);
	
	dealloc_SGS_MHD_control_c(mhd_ctl);
	free(mhd_ctl);
	
	return 0;
}
