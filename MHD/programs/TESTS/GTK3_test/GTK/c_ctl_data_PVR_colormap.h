/*
//  c_ctl_data_PVR_colormap.h
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#ifndef C_CTL_DATA_PVR_COLORMAP_H_
#define C_CTL_DATA_PVR_COLORMAP_H_

#include <stdio.h>
#include <stdlib.h> 
#include <string.h>

#include "skip_comment_c.h"
#include "t_control_c_lists.h"
#include "t_control_chara_IO.h"
#include "t_control_chara2_IO.h"
#include "t_control_chara_real_IO.h"
#include "t_ctl_array_real3_items_c.h"
#include "t_ctl_array_single_items_c.h"
#include "t_ctl_array_chara_real_items_c.h"
#include "t_ctl_array_chara2_items_c.h"
#include "t_ctl_data_pvr_colormap_c.h"
#include "c_ctl_data_PVR_view_matrix.h"



/* prototypes */

struct pvr_colormap_bar_ctl_c * init_f_PVR_colormap_bar_ctl(char * ctl_file_name, 
                                                            void *(*c_load_self)(void *f_parent), 
                                                            void *f_parent);
struct lighting_ctl_c * init_f_PVR_lighting_ctl(char * ctl_file_name, 
                                                void *(*c_load_self)(void *f_parent), 
                                                void *f_parent);


#endif /* C_CTL_DATA_PVR_COLORMAP_H_ */
