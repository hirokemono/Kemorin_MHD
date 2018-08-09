/*
//  main_control_LIC_test_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include <stdlib.h>
#include <stdio.h>

#include "t_control_data_LIC_pvr_c.h"


struct LIC_pvr_ctl_c *lic_pvr_c0;

int main(int argc,char *argv[])
{
	char buf[LENGTHBUF];      /* character buffer for reading line */
	char file_name[LENGTHBUF] = "/Users/matsui/work/C_test/ctl_pvr_BLIC";
	char file_name_2[LENGTHBUF];
	int iflag, i;
/*
    if (argc <= 1) {
		printf("Input control file in standard input. \n");
		return 1;
	};
	strngcopy(file_name, argv[1]);
*/
    strngcopy(file_name_2, file_name);
	strcat(file_name_2, "_2");
		
    printf("read LIC-PVR control file name: %s\n", file_name);
    printf("Write LIC-PVR control file name: %s\n", file_name_2);

    lic_pvr_c0 = (struct LIC_pvr_ctl_c *) malloc(sizeof(struct LIC_pvr_ctl_c));
	alloc_LIC_pvr_ctl_c(lic_pvr_c0);
	iflag = read_LIC_pvr_ctl_file_c(file_name, buf, lic_pvr_c0);
    
    rename_LIC_pvr_ctl_subfiles(lic_pvr_c0);
	iflag = write_LIC_pvr_ctl_file_c(file_name_2, lic_pvr_c0);

    dealloc_LIC_pvr_ctl_c(lic_pvr_c0);
	free(lic_pvr_c0);
	return 0;
}
