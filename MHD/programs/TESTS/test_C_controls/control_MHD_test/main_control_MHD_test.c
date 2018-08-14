#include <stdlib.h>
#include <stdio.h>

#include "all_field_names_c.h"
#include "control_elements_IO_c.h"
#include "t_SGS_MHD_control_c.h"

struct all_field_ctl_c **all_fld_list;


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
	
    all_fld_list = (struct all_field_ctl_c **) malloc(NUM_FIELD * sizeof(struct all_field_ctl_c *));
    alloc_all_field_ctl_c(all_fld_list);
		
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

    load_field_w_qflag_from_ctl(mhd_ctl->model_ctl->fld_ctl, all_fld_list);
    
    write_SGS_MHD_control_file_c(file_name_2, mhd_ctl);

    rename_SGS_MHD_ctl_subfile_c(mhd_ctl);
    write_SGS_MHD_ctl_subfile_c(mhd_ctl);

    dealloc_SGS_MHD_control_c(mhd_ctl);
    
    for(i=0;i<NUM_FIELD;i++){
        printf("%d:, %s: %s: %d %d %d\n", i, all_fld_list[i]->field_name,
               all_fld_list[i]->field_math, all_fld_list[i]->iflag_use,
               all_fld_list[i]->iflag_viz, all_fld_list[i]->iflag_monitor);
    };
    dealloc_all_field_ctl_c(all_fld_list);
    
	free(mhd_ctl);
	
	return 0;
}
