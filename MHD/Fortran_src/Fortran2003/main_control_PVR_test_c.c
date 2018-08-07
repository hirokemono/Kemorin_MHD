/*
//  main_control_PVR_test_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/03.
*/

#include <stdlib.h>
#include <stdio.h>

#include "t_control_data_4_pvr_c.h"


struct pvr_ctl_c *pvr_c0;

int main(int argc,char *argv[])
{
	char buf[LENGTHBUF];      /* character buffer for reading line */
	char file_name[LENGTHBUF] = "/Users/matsui/work/C_test/ctl_pvr_wz";
	char file_name_2[LENGTHBUF];
	int iflag, i;

    if (argc <= 1) {
		printf("Input control file in standard input. \n");
		return 1;
	};
	strngcopy(file_name, argv[1]);

    strngcopy(file_name_2, file_name);
	strcat(file_name_2, "_2");
		
    printf("read PSF control file name: %s\n", file_name);
    printf("Write PSF control file name: %s\n", file_name_2);

    pvr_c0 = (struct pvr_ctl_c *) malloc(sizeof(struct pvr_ctl_c));
	alloc_pvr_ctl_c(pvr_c0);
	iflag = read_pvr_ctl_file_c(file_name, buf, pvr_c0);
    

    if(pvr_c0->iflag_modeview_ctl ==-1){
        strcat(pvr_c0->pvr_modelview_file_name, "_2");
    }
    if(pvr_c0->iflag_colormap_ctl ==-1){
        strcat(pvr_c0->pvr_colormap_file_name, "_2");
    }
    for (i=0;i<pvr_c0->num_pvr_sect_ctl;i++){
        if(pvr_c0->pvr_sect_ctl[i]->iflag_psf_define_ctl == -1){
            strcat(pvr_c0->pvr_sect_ctl[i]->fname_sect_ctl, "_2");
        };
    };
    
	iflag = write_pvr_ctl_file_c(file_name_2, pvr_c0);

    dealloc_pvr_ctl_c(pvr_c0);
	free(pvr_c0);
	return 0;
}
