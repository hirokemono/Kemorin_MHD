/*
//  c_ctl_data_PVR_view_matrix.h
//  Control_GTK
//
//  Created by Hiroaki Matsui on 7/5/23.
*/

#ifndef C_CTL_DATA_PVR_VIEW_MATRIX_H_
#define C_CTL_DATA_PVR_VIEW_MATRIX_H_

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
#include "t_ctl_array_chara2_real_items_c.h"
#include "t_ctl_data_4_view_transfer_c.h"



/* prototypes */

struct modelview_ctl_c * init_f_VIZ_view_matrix_ctl(char *ctl_file_name,
                                                    void *(*c_load_self)(void *f_parent), 
                                                    void *f_parent);


#endif /* C_CTL_DATA_PVR_VIEW_MATRIX_H_ */
