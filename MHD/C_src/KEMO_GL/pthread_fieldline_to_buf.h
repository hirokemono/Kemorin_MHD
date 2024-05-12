/*
 *  pthread_fieldline_to_buf.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#ifndef PTHREAD_FIELDLINE_TO_BUF_
#define PTHREAD_FIELDLINE_TO_BUF_

#include "calypso_param_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_fline_data_4_viewer_c.h"
#include "m_kemoview_fline_menu.h"
#include "m_vertex_buffer.h"
#include "set_fieldline_to_buf.h"

long sel_fieldtubes_to_buf_pthread(long ist_patch, const int nthreads, 
                                   struct fline_data *fline_d,
                                   struct fline_menu_val *fline_m,
                                   struct gl_strided_buffer *strided_buf);
long sel_fieldlines_to_buf_pthread(long ist_patch, const int nthreads, 
                                   struct fline_data *fline_d,
                                   struct fline_menu_val *fline_m,
                                   struct gl_strided_buffer *strided_buf);

#endif /* PTHREAD_FIELDLINE_TO_BUF_ */
