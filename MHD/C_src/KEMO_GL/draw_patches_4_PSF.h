
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
#include "set_PSF_patches_to_buf.h"
#include "set_PSF_isolines_to_buf.h"
#include "modify_object_4_viewer_c.h"


/* prptotypes */

int check_draw_psf(struct kemo_array_control *psf_a);

void const_PSF_patch_buffer(int shading_mode, long ist_psf, long ied_psf,
                            struct psf_data **psf_s, struct psf_menu_val **psf_m,
                            struct kemo_array_control *psf_a,
                            struct gl_strided_buffer *psf_buf);
void const_PSF_texture_buffer(int shading_mode, long ist_psf, long ied_psf,
                              struct psf_data **psf_s, struct psf_menu_val **psf_m,
                              struct kemo_array_control *psf_a,
                              struct gl_strided_buffer *psf_buf);

void const_PSF_solid_objects_buffer(struct view_element *view_s, struct psf_data **psf_s,
                                    struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                                    struct gl_strided_buffer *PSF_solid_buf,
                                    struct gl_strided_buffer *PSF_stxur_buf,
                                    struct gl_strided_buffer *PSF_isoline_buf,
                                    struct gl_strided_buffer *PSF_arrow_buf);
void const_PSF_trans_objects_buffer(struct view_element *view_s, struct psf_data **psf_s,
                                    struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
                                    struct gl_strided_buffer *PSF_trns_buf,
                                    struct gl_strided_buffer *PSF_ttxur_buf);
#endif
