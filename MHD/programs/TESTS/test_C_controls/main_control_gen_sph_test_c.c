/*
//  main_control_gen_sph_test_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include <stdlib.h>
#include <stdio.h>

#include "t_control_data_gen_sph_grid_c.h"


struct gen_sph_grid_ctl_c *gen_sph_c0;

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

    gen_sph_c0 = init_gen_sph_shell_ctl_c();
    
	iflag = read_gen_sph_shell_file_c(file_name, buf, gen_sph_c0);
    rename_gen_sph_subfiles_c(gen_sph_c0);
	iflag = write_gen_sph_shell_file_c(file_name_2, gen_sph_c0);

    dealloc_gen_sph_shell_ctl_c(gen_sph_c0);
	return 0;
}
