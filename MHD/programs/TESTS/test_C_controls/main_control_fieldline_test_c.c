/*
//  main_control_fieldline_test_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include <stdlib.h>
#include <stdio.h>

#include "t_control_data_4_fline_c.h"


struct fline_ctl_c *fline_c0;

int main(int argc,char *argv[])
{
	char buf[LENGTHBUF];      /* character buffer for reading line */
	char file_name[LENGTHBUF] = "/Users/matsui/work/C_test/ctl_fline_magne";
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
	
	fline_c0 = init_fline_ctl_c();
	
	iflag = read_fline_ctl_file_c(file_name, buf, fline_c0);
	iflag = write_fline_ctl_file_c(file_name_2, fline_c0);
    dealloc_fline_ctl_c(fline_c0);
	return 0;
}
