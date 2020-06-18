/*
//  main_view_control_test_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include <stdlib.h>
#include <stdio.h>

#include "t_ctl_data_4_view_transfer_c.h"


struct modelview_ctl_c *mat_c0;

int main(int argc,char *argv[])
{
	char buf[LENGTHBUF];      /* character buffer for reading line */
	char file_name[LENGTHBUF] = "ctl_view";
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
	
	mat_c0 = (struct modelview_ctl_c *) malloc(sizeof(struct modelview_ctl_c));
	alloc_modelview_ctl_c(mat_c0);
	
	iflag = read_modelview_file_c(file_name, buf, mat_c0);
	iflag = write_modelview_file_c(file_name_2, mat_c0);
	free(mat_c0);
	return 0;
}
