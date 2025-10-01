/*
 *  set_color_code_on_nodes.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/15.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#ifndef SET_COLOR_CODE_ON_NODES_
#define SET_COLOR_CODE_ON_NODES_

#include "m_psf_data_4_viewer_c.h"
#include "m_fline_data_4_viewer_c.h"
#include "m_kemoview_psf_menu.h"
#include "m_kemoview_fline_menu.h"
#include "rainbow_color_code_c.h"

/*  prototypes */

void set_color_code_for_psfs(struct psf_data **psf_s, struct psf_menu_val **psf_m, 
                             struct kemo_array_control *psf_a);
void set_color_code_for_fieldlines(struct psf_data *fline_d,
								   struct psf_menu_val *fline_m);

#endif

