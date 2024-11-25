/*
//  set_each_fline_parameters.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/

#ifndef SET_EACH_FLINE_PARAMETERS_
#define SET_EACH_FLINE_PARAMETERS_

#include <stdio.h>

#include "kemoviewer.h"
#include "m_fline_data_4_viewer_c.h"
#include "m_kemoview_fline_menu.h"
#include "skip_comment_c.h"
#include "kemoviewer_base.h"
#include "read_data_4_kemoviewer.h"

/* prototypes */


void set_fline_file_step(struct psf_menu_val *fline_m, int istep);


void set_fline_type(struct psf_menu_val *fline_m, long iflag);
long get_fline_type(struct psf_menu_val *fline_m);

#endif
