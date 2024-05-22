/*
// set_PSF_isolines_to_buf.h
*/

#ifndef SET_PSF_ISOLINES_TO_BUF_
#define SET_PSF_ISOLINES_TO_BUF_

#include "kemoviewer_param_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "t_PSF_each_isoline_edge_list.h"
#include "set_new_patch_4_map_c.h"
#include "set_color_code_on_nodes.h"
#include "rainbow_color_code_c.h"
#include "icosahedron_c.h"
#include "pthread_each_isoline_to_buf.h"

/* prptotypes */

double cal_isoline_value(int j, int n_isoline, struct colormap_params *cmap_s);
int find_start_positive_lines(int n_isoline, struct colormap_params *cmap_s);

long add_PSF_all_isolines_num(const long ist_patch, const int nthreads,
                              struct psf_data *psf_s, struct psf_menu_val *psf_m,
                              long *istack_smp_psf_iso);

long set_PSF_all_isotubes_to_buf(const long ist_patch,
                                 const int nthreads,
                                 long *istack_smp_psf_iso,
                                 int isoline_ncorner, double isoline_width,
                                 struct psf_data *psf_s,
                                 struct psf_menu_val *psf_m,
                                struct gl_strided_buffer *psf_buf);
long set_PSF_all_isolines_to_buf(const long ist_patch,
                                 const int nthreads, long *istack_smp_psf_iso,
                                 struct psf_data *psf_s, struct psf_menu_val *psf_m,
                                 struct gl_strided_buffer *psf_buf);
#endif
