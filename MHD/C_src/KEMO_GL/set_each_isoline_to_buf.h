/*
 *  set_each_isoline_to_buf.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#ifndef SET_EACH_ISOLINE_TO_BUF_
#define SET_EACH_ISOLINE_TO_BUF_

#include "m_psf_data_4_viewer_c.h"
#include "set_hex_tube_to_buf.h"
#include "set_new_patch_4_map_c.h"

/* prototypes */

int add_each_isoline_npatch(int ist_patch, double v_line, int icomp, struct psf_data *psf_s);
int set_each_isoline_to_buf(int ist_patch, double width, 
                            double v_line, int icomp, double *f_color,
                            struct psf_data *psf_s, struct gl_strided_buffer *strided_buf);
int set_each_map_isoline_to_buf(int ist_patch, double width, 
							   double v_line, int icomp, double *f_color, 
							   struct psf_data *psf_s, struct gl_strided_buffer *strided_buf);


/* SET_EACH_ISOLINE_TO_BUF_ */
#endif

