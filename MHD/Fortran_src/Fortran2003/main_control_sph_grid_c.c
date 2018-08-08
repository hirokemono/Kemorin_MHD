/*
//  main_control_sph_grid_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include <stdlib.h>
#include <stdio.h>

#include "t_control_data_sph_grid_c.h"


struct parallel_sph_shell_control_c *shell_ctl0;

int main(int argc,char *argv[])
{
	char buf[LENGTHBUF];      /* character buffer for reading line */
	char file_name[LENGTHBUF] = "/Users/matsui/work/C_test/control_sph_shell";
	char file_name_2[LENGTHBUF];
	int iflag;
	
    if (argc <= 1) {
		printf("Input control file in standard input. \n");
		return 1;
	};
	strngcopy(file_name, argv[1]);
	
    strngcopy(file_name_2, file_name);
	strcat(file_name_2, "_2");
		
    printf("read visualizer control file name: %s\n", file_name);
    printf("Write visualizer control file name: %s\n", file_name_2);

    shell_ctl0 = (struct parallel_sph_shell_control_c *) malloc(sizeof(struct parallel_sph_shell_control_c));
	alloc_parallel_sph_shell_control_c(shell_ctl0);
    
	iflag = read_spherical_shell_file_c(file_name, buf, shell_ctl0);
	iflag = write_spherical_shell_file_c(file_name_2, shell_ctl0);

    dealloc_parallel_sph_shell_control_c(shell_ctl0);
	free(shell_ctl0);
	return 0;
}
