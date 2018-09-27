/*
//  load_MHD_control_c.h
//  
//
//  Created by Hiroaki Matsui on 2018/09/07.
*/

#ifndef load_MHD_control_c_h_
#define load_MHD_control_c_h_

#include <stdio.h>
#include "t_SGS_MHD_control_c.h"

void load_MHD_control_c();


struct SGS_MHD_control_c * link_to_mhd_ctl();
struct PVR_ctl_list * link_to_pvr_ctl_list();


#endif /* load_MHD_control_c_h_ */
