/*
//  main_control_VIZ_test_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include <stdlib.h>
#include <stdio.h>

#include "t_control_data_visualizer_c.h"


struct viz_only_ctl_c *viz_only0;

int main(int argc,char *argv[])
{
	char buf[LENGTHBUF];      /* character buffer for reading line */
	char file_name[LENGTHBUF] = "/Users/matsui/work/C_test/ctl_viz";
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

    viz_only0 = (struct viz_only_ctl_c *) malloc(sizeof(struct viz_only_ctl_c));
	alloc_visualizers_ctl_c(viz_only0);
    
	iflag = read_visualizers_ctl_file_c(file_name, buf, viz_only0);
    rename_visualizer_subfiles_c(viz_only0);
	iflag = write_visualizers_ctl_file_c(file_name_2, viz_only0);

    dealloc_visualizers_ctl_c(viz_only0);
	free(viz_only0);
	return 0;
}
