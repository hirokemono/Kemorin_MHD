/*
//  set_each_arrow_line_c.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/


#ifndef SET_EACH_ARROW_LINE_C_
#define SET_EACH_ARROW_LINE_C_

#include "kemoviewer.h"
#include "kemoviewer_base.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_kemoview_psf_menu.h"

/* prototypes */

void set_line_for_tracer_arrow(int icomp, long inod, 
                               struct psf_data *psf_s,
                               struct psf_menu_val *psf_m,
                               double xyzw_line[8], 
                               double dir_line[8]);
void set_line_for_psf_arrow(int icomp, long inod, 
                            struct psf_data *psf_s,
                            struct psf_normals *psf_n,
                            struct psf_menu_val *psf_m,
                            double xyzw_line[8], 
                            double dir_line[8]);

#endif /*  SET_EACH_ARROW_LINE_C_  */
