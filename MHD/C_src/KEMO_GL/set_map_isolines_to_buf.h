/*
 *  set_map_isolines_to_buf.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/16.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#ifndef SET_MAP_ISOLINES_TO_BUF_
#define SET_MAP_ISOLINES_TO_BUF_

#include "kemoviewer_param_c.h"
#include "m_kemoview_psf_menu.h"
#include "m_psf_data_4_viewer_c.h"
#include "t_PSF_each_isoline_edge_list.h"
#include "set_new_patch_4_map_c.h"
#include "set_color_code_on_nodes.h"
#include "rainbow_color_code_c.h"
#include "coordinate_converter_c.h"
#include "set_PSF_isolines_to_buf.h"
#include "pthread_each_isoline_to_buf.h"
#include "icosahedron_c.h"

/* prototypes */

long add_map_PSF_isoline(const long ist_patch, const int nthreads,
                         struct psf_data *psf_s, struct psf_menu_val *psf_m,
                         long *istack_smp_map_iso);
long set_map_PSF_isoline_to_buf(const long ist_patch,
                                const int nthreads, long *istack_smp_psf_iso,
                                const int isoline_ncorner, const double isoline_width,
                                struct psf_data *psf_s, struct psf_menu_val *psf_m,
                                struct gl_strided_buffer *psf_buf);

#endif
