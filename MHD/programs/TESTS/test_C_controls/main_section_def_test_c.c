/*
//  main_section_def_test_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include <stdlib.h>
#include <stdio.h>

#include "t_control_data_4_psf_c.h"


struct control_labels_f *label_psf_ctl0;
struct psf_define_ctl_c *psf_def_c0;

int main(int argc,char *argv[])
{
	char buf[LENGTHBUF];      /* character buffer for reading line */
	char file_name[LENGTHBUF] = "/Users/matsui/work/C_test/ctl_psf_eq_def";
	char file_name_2[LENGTHBUF];
	int iflag;
	
	if (argc <= 1) {
		printf("Input control file in standard input. \n");
		return 1;
	};
	strngcopy(file_name, argv[1]);
    
	strngcopy(file_name_2, file_name);
	strcat(file_name_2, "_2");
	
	printf("Input file name: %s\n", file_name);
	printf("Copied file name: %s\n", file_name_2);
	
	psf_def_c0 = init_psf_define_ctl_c();
    label_psf_ctl0 = init_label_psf_ctl();
	
	iflag = read_psf_define_file_c(file_name, buf, label_psf_ctl0, psf_def_c0);
	iflag = write_psf_define_file_c(file_name_2, label_psf_ctl0, psf_def_c0);
    dealloc_psf_define_ctl_c(psf_def_c0);
	return 0;
}
