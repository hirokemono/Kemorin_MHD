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

long sel_each_isoline_to_list_pthread(const int nthreads, long *istack_threads,
                                      double v_line, long icomp,
                                      struct psf_data *psf_s, struct psf_normals *psf_n,
                                      struct isoline_line_work *wk_iso_line);
long sel_each_map_isoline_to_list_pthread(const int nthreads, long *istack_threads,
                                          double v_line, long icomp,
                                          struct psf_data *psf_s,
                                          struct psf_normals *psf_n,
                                          struct isoline_line_work *wk_iso_line);

long sel_each_isotube_to_buf_pthread(const long ist_patch, long ntot_line,
                                     const int nthreads, long *istack_threads,
                                     struct psf_data *psf_s,
                                     struct isoline_line_work *wk_iso_line,
                                     struct gl_strided_buffer *strided_buf,
                                     struct gl_index_buffer *index_buf);
long sel_each_isoline_to_buf_pthread(const long ist_patch, long ntot_line,
                                     const int nthreads, long *istack_threads,
                                     struct psf_data *psf_s,
                                     struct isoline_line_work *wk_iso_line,
                                     struct gl_strided_buffer *strided_buf);

#endif /* PTHREAD_EACH_ISOLINE_TO_BUF_ */


