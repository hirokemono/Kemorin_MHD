/*
 *  pthread_each_isoline_to_buf.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#ifndef PTHREAD_EACH_ISOLINE_TO_BUF_
#define PTHREAD_EACH_ISOLINE_TO_BUF_

#include "calypso_param_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "set_each_isoline_to_buf.h"


/* prototypes */

long sel_add_each_isoline_npatch_pthread(const long ist_patch, const int nthreads,
                                         double v_line, long icomp, struct psf_data *psf_s,
                                         long *istack_threads);

long sel_each_isoline_to_buf_pthread(const long ist_patch,
                                     const int nthreads, long *istack_threads,
                                     double width, double v_line,
                                     long icomp, double *f_color,
                                     struct psf_data *psf_s,
                                     struct gl_strided_buffer *strided_buf);

long sel_each_map_isoline_to_buf_pthread(const long ist_patch,
                                         const int nthreads, long *istack_threads,
                                         double width, double v_line,
                                         long icomp, double *f_color,
                                         struct psf_data *psf_s,
                                         struct gl_strided_buffer *strided_buf);

#endif /* PTHREAD_EACH_ISOLINE_TO_BUF_ */


