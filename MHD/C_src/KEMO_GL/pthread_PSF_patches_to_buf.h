/*
 *  pthread_PSF_patches_to_buf.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#ifndef PTHREAD_PSF_PATCHES_TO_BUF_
#define PTHREAD_PSF_PATCHES_TO_BUF_

#include "calypso_param_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "set_PSF_patches_to_buf.h"

long sel_psf_nodes_to_buf_pthread(long ipatch_in, const int nthreads,
                                  long ist_psf, long num,
                                  int i_psf, struct psf_data **psf_s,
                                  struct gl_strided_buffer *strided_buf);
long sel_psf_patches_to_buf_pthread(long ipatch_in, const int nthreads,
                                    long ist_psf, long ied_psf,
                                    struct psf_data **psf_s,
                                    struct kemo_array_control *psf_a,
                                    struct gl_strided_buffer *strided_buf);

long sel_psf_textures_to_buf_pthread(int nthreads, long ist_psf, long ied_psf,
                                     struct psf_data **psf_s, struct kemo_array_control *psf_a,
                                     struct gl_strided_buffer *strided_buf);

#endif /* PTHREAD_PSF_PATCHES_TO_BUF_ */


