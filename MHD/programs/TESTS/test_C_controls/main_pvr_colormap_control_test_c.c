/*
//  main_pvr_colormap_control_test_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include <stdlib.h>
#include <stdio.h>

#include "t_ctl_data_pvr_colormap_c.h"


struct pvr_colormap_bar_ctl_c *cmap_cbar_c0;

int main(int argc,char *argv[])
{
	char buf[LENGTHBUF];      /* character buffer for reading line */
	char file_name[LENGTHBUF] = "/Users/matsui/work/C_test/ctl_pvr_colormap";
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
	
	cmap_cbar_c0 = (struct pvr_colormap_bar_ctl_c *) malloc(sizeof(struct pvr_colormap_bar_ctl_c));
	alloc_colormap_colorbar_ctl_c(cmap_cbar_c0);
	
	iflag = read_colormap_colorbar_ctl_c(file_name, buf, cmap_cbar_c0);
	iflag = write_colormap_colorbar_ctl_c(file_name_2, cmap_cbar_c0);
    dealloc_colormap_colorbar_ctl_c(cmap_cbar_c0);
	free(cmap_cbar_c0);
	return 0;
}
