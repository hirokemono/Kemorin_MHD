/*
 *  pthread_PSF_arrows_to_buf.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 08/12/20.
 *  Copyright 2020 Dept. of Earth and Planetary Sciences, UC Davis. All rights reserved.
 *
 */

#ifndef PTHREAD_PSF_ARROWS_TO_BUF_
#define PTHREAD_PSF_ARROWS_TO_BUF_

#include "calypso_param_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "set_PSF_patches_to_buf.h"

long sel_add_num_psf_arrows_pthread(long ist_cone, const int nthreads,
                                    long *istack_arrow, int ncorner, 
                                    struct psf_data *psf_s, struct psf_menu_val *psf_m);

long sel_psf_arrows_to_buf_pthread(long ist_cone,
                                   const int nthreads, long *istack_smp_arrow,
                                   int ncorner, double radius,
                                   struct psf_data *psf_s, struct psf_menu_val *psf_m,
                                   struct gl_strided_buffer *strided_buf);

#endif /* PTHREAD_PSF_ARROWS_TO_BUF_ */


