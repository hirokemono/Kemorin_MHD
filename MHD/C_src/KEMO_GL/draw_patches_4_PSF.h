
/* draw_patches_4_PSF.h */

#ifndef DRAW_PATCHES_4_PSF_
#define DRAW_PATCHES_4_PSF_

#include "kemoviewer_param_c.h"
#include "m_kemoview_psf_menu.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_gl_transfer_matrix.h"
#include "m_vertex_buffer.h"
#include "set_new_patch_4_map_c.h"
#include "icosahedron_c.h"
#include "coordinate_converter_c.h"
#include "set_color_code_on_nodes.h"
#include "pthread_PSF_patches_to_buf.h"
#include "pthread_PSF_arrows_to_buf.h"
#include "set_PSF_isolines_to_buf.h"
#include "modify_object_4_viewer_c.h"
#include "set_axis_to_buf.h"


/* prptotypes */

int check_draw_psf(struct kemo_array_control *psf_a);

void const_PSF_node_stack(struct psf_data **psf_s,
                          struct kemo_array_control *psf_a);
void const_PSF_node_buffer(const int nthreads,
                           struct psf_data **psf_s,
                           struct psf_normals **psf_n,
                           struct kemo_array_control *psf_a,
                           struct gl_strided_buffer *psf_buf);

void const_PSF_patch_index_buffer(const int nthreads, long ist_psf, long ied_psf,
                                  struct psf_data **psf_s,  struct kemo_array_control *psf_a,
                                  struct gl_index_buffer *index_buf);

void const_PSF_texture_buffer(int shading_mode, const int nthreads,
                              long ist_psf, long ied_psf,
                              struct psf_data **psf_s,
                              struct psf_normals **psf_n,
                              struct psf_menu_val **psf_m,
                              struct kemo_array_control *psf_a,
                              struct gl_strided_buffer *psf_buf);
void const_PSF_patch_buffer(const int nthreads, long ist_psf, long ied_psf,
                            struct psf_data **psf_s,
                            struct psf_normals **psf_n,
                            struct kemo_array_control *psf_a,
                            struct gl_strided_buffer *psf_buf);


void const_PSF_isotube_buffer(const int nthreads,
                              struct view_element *view_s,
                              struct psf_data **psf_s,
                              struct psf_normals **psf_n,
                              struct psf_menu_val **psf_m,
                              struct kemo_array_control *psf_a,
                              struct gl_strided_buffer *mline_buf,
                              struct gl_index_buffer *index_buf);
void const_PSF_isoline_buffer(const int nthreads, struct view_element *view_s,
                              struct psf_data **psf_s, struct psf_normals **psf_n,
                              struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                              struct gl_strided_buffer *psf_buf);
void const_PSF_arrow_buffer(const int nthreads, struct view_element *view_s,
                            struct psf_data **psf_s,
                            struct psf_normals **psf_n,
                            struct psf_menu_val **psf_m,
                            struct kemo_array_control *psf_a,
                            struct gl_strided_buffer *psf_buf,
                            struct gl_index_buffer *psf_index_buf);
#endif
