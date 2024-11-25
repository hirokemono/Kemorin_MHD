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

#include "calypso_param_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "t_PSF_each_isoline_edge_list.h"
#include "set_hex_tube_to_buf.h"
#include "set_new_patch_4_map_c.h"
#include "set_primitives_to_gl_buffer.h"


/* prototypes */

long count_each_isoline_npatch(const long ist, const long ied,
                               const double v_line, long icomp,
                               struct psf_data *psf_s);

long set_each_isoline_to_list(const long ist_line,
                              const long ist, const long ied,
                              double v_line, long icomp,
                              struct psf_data *psf_s,
                              struct psf_normals *psf_n,
                              struct isoline_line_work *wk_iso_line);
long set_each_map_isoline_to_list(const long ist_line,
                                  const long ist, const long ied,
                                  double v_line, long icomp,
                                  struct psf_data *psf_s,
                                  struct psf_normals *psf_n,
                                  struct isoline_line_work *wk_iso_line);


long set_each_isotube_to_buf(const long ist_tube, const long ist, const long ied,
                             struct psf_data *psf_s,
                             struct isoline_line_work *wk_iso_line,
                             struct gl_strided_buffer *strided_buf,
                             struct gl_index_buffer *index_buf);

long set_each_isoline_to_buf(const long ist_tube,
                             const long ist, const long ied,
                             struct psf_data *psf_s,
                             struct isoline_line_work *wk_iso_line,
                             struct gl_strided_buffer *strided_buf);
/* SET_EACH_ISOLINE_TO_BUF_ */
#endif

