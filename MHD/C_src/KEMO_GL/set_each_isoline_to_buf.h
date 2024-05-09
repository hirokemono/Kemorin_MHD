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

#include <pthread.h>
#include "calypso_param_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "set_hex_tube_to_buf.h"
#include "set_new_patch_4_map_c.h"

typedef struct{
    int id;
    int nthreads;
    
    struct gl_strided_buffer        *strided_buf;
    struct gl_local_buffer_address  *point_buf;
    struct psf_data *psf_s;

    long icomp;
    double width;
    double v_line;
    double *f_color;

    long ist_patch;
    long *num_line;
} args_pthread_PSF_Isoline;


/* prototypes */

long add_each_isoline_npatch(const long ist_patch, const long ist, const long ied,
                             const double v_line, long icomp, struct psf_data *psf_s);
long set_each_isoline_to_buf(const long ist_patch,
                             const long ist, const long ied,
                             double width, double v_line,
                             long icomp, double *f_color,
                             struct psf_data *psf_s,
                             struct gl_strided_buffer *strided_buf,
                             struct gl_local_buffer_address *point_buf);
long set_each_map_isoline_to_buf(const long ist_patch,
                                 const long ist, const long ied,
                                 double width, double v_line,
                                 long icomp, double *f_color,
                                 struct psf_data *psf_s,
                                 struct gl_strided_buffer *strided_buf,
                                 struct gl_local_buffer_address *point_buf);


long add_each_isoline_npatch_pthread(const long ist_patch, const int nthreads,
                                     double v_line, long icomp, struct psf_data *psf_s,
                                     long *istack_threads);

long set_each_isoline_to_buf_pthread(const long ist_patch,
                                     const int nthreads, long *istack_threads,
                                     double width, double v_line,
                                     long icomp, double *f_color,
                                     struct psf_data *psf_s,
                                     struct gl_strided_buffer *strided_buf,
                                     struct gl_local_buffer_address **para_point_buf);

long set_each_map_isoline_to_buf_pthread(const long ist_patch,
                                         const int nthreads, long *istack_threads,
                                         double width, double v_line,
                                         long icomp, double *f_color,
                                         struct psf_data *psf_s,
                                         struct gl_strided_buffer *strided_buf,
                                         struct gl_local_buffer_address **para_point_buf);
/* SET_EACH_ISOLINE_TO_BUF_ */
#endif

