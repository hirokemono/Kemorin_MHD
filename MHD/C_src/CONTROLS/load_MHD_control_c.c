/*
//  load_MHD_control_c.c
//  
//
//  Created by Hiroaki Matsui on 2018/09/07.
*/

#include "load_MHD_control_c.h"

struct SGS_MHD_control_c *mhd_ctl;
char file_name[LENGTHBUF] = "/Users/matsui/work/C_test/control_MHD";

void load_MHD_control_c(){
    char buf[LENGTHBUF];      /* character buffer for reading line */
    
    mhd_ctl = alloc_SGS_MHD_control_c();
    read_SGS_MHD_control_file_c(file_name, buf, mhd_ctl);

    return;
};

struct SGS_MHD_control_c * link_to_mhd_ctl(){
    return mhd_ctl;
}

struct PVR_ctl_list * link_to_pvr_ctl_list(){
    return &mhd_ctl->viz_c->pvr_ctl_list;
}
