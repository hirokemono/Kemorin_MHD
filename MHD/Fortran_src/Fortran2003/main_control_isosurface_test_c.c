/*
//  main_control_isosurface_test_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include <stdlib.h>
#include <stdio.h>

#include "t_control_data_4_iso_c.h"


struct iso_ctl_c *iso_c0;

int main(int argc,char *argv[])
{
	char buf[LENGTHBUF];      /* character buffer for reading line */
	char file_name[LENGTHBUF] = "/Users/matsui/work/C_test/ctl_iso_press";
	char file_name_2[LENGTHBUF];
	int iflag;

    if (argc <= 1) {
		printf("Input control file in standard input. \n");
		return 1;
	};
	strngcopy(file_name, argv[1]);
    
	strngcopy(file_name_2, file_name);
	strcat(file_name_2, "_2");
		
    printf("read isosurface control file name: %s\n", file_name);
    printf("Write isosurface control file name: %s\n", file_name_2);

    iso_c0 = (struct iso_ctl_c *) malloc(sizeof(struct iso_ctl_c));
	alloc_iso_ctl_c(iso_c0);
	
	iflag = read_iso_ctl_file_c(file_name, buf, iso_c0);
	iflag = write_iso_ctl_file_c(file_name_2, iso_c0);
    
    dealloc_iso_ctl_c(iso_c0);
	free(iso_c0);
	return 0;
}
