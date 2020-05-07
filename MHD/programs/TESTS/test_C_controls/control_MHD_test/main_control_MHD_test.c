#include <stdlib.h>
#include <stdio.h>

#include "control_elements_IO_c.h"
#include "t_SGS_MHD_control_c.h"
#include "m_field_name_from_f.h"

struct all_field_ctl_c *all_fld_list;


int main(int argc,char *argv[])
{
	struct SGS_MHD_control_c *mhd_ctl;
    struct field_names_f *fld_l = init_field_name_f();

	char buf[LENGTHBUF];      /* character buffer for reading line */
	char file_name[LENGTHBUF] = "/Users/matsui/work/C_test/control_MHD";
	char file_name_2[LENGTHBUF];
	int i;
    
    int num_comps;
	
    all_fld_list = init_all_field_ctl_c();
		
    printf("baka %d\n", fld_l->ntot_fields);
	for(i=0;i<fld_l->ntot_fields;i++){
		printf("field_name %d: %s %d %s\n", i, fld_l->field_name[i], fld_l->num_comp[i], fld_l->field_math[i]);
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
	
	
	mhd_ctl = alloc_SGS_MHD_control_c();
	
	read_SGS_MHD_control_file_c(file_name, buf, mhd_ctl);

    load_field_w_qflag_from_ctl(mhd_ctl->model_ctl->fld_ctl, all_fld_list);
    
    write_SGS_MHD_control_file_c(file_name_2, mhd_ctl);

    rename_SGS_MHD_ctl_subfile_c(mhd_ctl);
    write_SGS_MHD_ctl_subfile_c(mhd_ctl);

    dealloc_SGS_MHD_control_c(mhd_ctl);
    
    for(i=0;i<fld_l->ntot_fields;i++){
        printf("%d:, %s: %s: %d %d %d\n", i, all_fld_list->fld_list->field_name[i],
               all_fld_list->fld_list->field_math[i], all_fld_list->iflag_use[i],
               all_fld_list->iflag_viz[i], all_fld_list->iflag_monitor[i]);
    };
    dealloc_all_field_ctl_c(all_fld_list);
    
	free(mhd_ctl);
	
	return 0;
}
