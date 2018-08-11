#include <stdlib.h>
#include <stdio.h>

#include "all_field_names_c.h"
#include "control_elements_IO_c.h"
#include "t_SGS_MHD_control_c.h"

struct field_control{
	struct field_def *fld_def;
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
    char fld_name[NCHARA_FIELD];
    char fld_math[KCHARA_C];
	int i;
    
    int num_comps;
	
	fld_ctl = (struct field_control *) malloc(sizeof(struct field_control));
	fld_ctl->fld_def = (struct field_def *)malloc(sizeof(struct field_def));
	fld_ctl->iflag_use = (int *)calloc(NUM_FIELD, sizeof(int));
	fld_ctl->iflag_viz = (int *)calloc(NUM_FIELD, sizeof(int));
	fld_ctl->iflag_monitor = (int *)calloc(NUM_FIELD, sizeof(int));
		
    printf("baka %d\n", NUM_FIELD);
	for(i=0;i<NUM_FIELD;i++){
        num_comps = get_field_properties(i, fld_name, fld_math);
		printf("field_name %d: %s %d %s\n", i, fld_name, num_comps, fld_math);
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
	write_SGS_MHD_control_file_c(file_name_2, mhd_ctl);

    rename_SGS_MHD_ctl_subfile_c(mhd_ctl);
    write_SGS_MHD_ctl_subfile_c(mhd_ctl);

	dealloc_SGS_MHD_control_c(mhd_ctl);
	free(mhd_ctl);
	
	return 0;
}
