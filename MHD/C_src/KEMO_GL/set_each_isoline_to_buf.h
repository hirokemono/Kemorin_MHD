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
#include "set_hex_tube_to_buf.h"
#include "set_new_patch_4_map_c.h"


struct isoline_mesh_work{
    long num_edge;
    long *inum_line;
    long *ineib_edge;
    double *xyzw_edge;
};
    
struct isoline_line_work{
    long num_line;
    long *iedge_itp;
    double *xyzw_line;
    double *dir_line;
    double *norm_line;
    double f_color[8];
    double width;
};

/* prototypes */

long add_each_isoline_npatch(const long ist_patch,
                             const long ist, const long ied,
                             const double v_line, long icomp,
                             struct psf_data *psf_s);
long set_each_isoline_to_buf(const long ist_patch,
                             const long ist, const long ied,
                             double width, double v_line,
                             long icomp, double *f_color,
                             struct psf_data *psf_s,
                             struct gl_strided_buffer *strided_buf);
long set_each_map_isoline_to_buf(const long ist_patch,
                                 const long ist, const long ied,
                                 double width, double v_line,
                                 long icomp, double *f_color,
                                 struct psf_data *psf_s,
                                 struct gl_strided_buffer *strided_buf);


long set_each_isoline_test(const long ist_line,
                             const long ist, const long ied,
                             double width, double v_line,
                             long icomp, double *f_color,
                             struct psf_data *psf_s,
                            long *iedge_itp,
                           double *xyzw_line);
long set_each_isoline_to_buf2(const long ist_patch,
                             const long ist, const long ied,
                             struct psf_data *psf_s,
                              struct isoline_line_work *wk_iso_line,
                             struct gl_strided_buffer *strided_buf);
/* SET_EACH_ISOLINE_TO_BUF_ */
#endif

