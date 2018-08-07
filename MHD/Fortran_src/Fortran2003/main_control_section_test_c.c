/*
//  main_control_section_test_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include <stdlib.h>
#include <stdio.h>

#include "t_control_data_4_psf_c.h"


struct psf_ctl_c *psf_c0;

int main(int argc,char *argv[])
{
	char buf[LENGTHBUF];      /* character buffer for reading line */
	char file_name[LENGTHBUF] = "/Users/matsui/work/C_test/ctl_psf_z0";
	char file_name_2[LENGTHBUF];
	int iflag;
	
	if (argc <= 1) {
		printf("Input control file in standard input. \n");
		return 1;
	};
	strngcopy(file_name, argv[1]);
    
	strngcopy(file_name_2, file_name);
	strcat(file_name_2, "_2");
		
    printf("read PSF control file name: %s\n", file_name);
    printf("Write PSF control file name: %s\n", file_name_2);

    psf_c0 = (struct psf_ctl_c *) malloc(sizeof(struct psf_ctl_c));
	alloc_psf_ctl_c(psf_c0);
	
	iflag = read_psf_ctl_file_c(file_name, buf, psf_c0);
    rename_psf_define_file_c(psf_c0);
	iflag = write_psf_ctl_file_c(file_name_2, psf_c0);
    
    dealloc_psf_ctl_c(psf_c0);
	free(psf_c0);
	return 0;
}
