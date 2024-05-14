
/* set_PSF_patches_to_buf.h */

#ifndef SET_PSF_PATCHES_TO_BUF_
#define SET_PSF_PATCHES_TO_BUF_

#include "kemoviewer_param_c.h"
#include "m_kemoview_psf_menu.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_vertex_buffer.h"
#include "set_new_patch_4_map_c.h"
#include "coordinate_converter_c.h"
#include "set_color_code_on_nodes.h"
#include "icosahedron_c.h"

/* prptotypes */

long count_psf_nodes_to_buf(long ist_psf, long ied_psf);

long set_psf_nodes_to_buf(long ipatch_in, long ist_psf, long ied_psf, int shading_mode, 
                          struct psf_data **psf_s, struct psf_menu_val **psf_m,
                          struct kemo_array_control *psf_a,
                          struct gl_strided_buffer *strided_buf);
long set_psf_textures_to_buf(long ist_texture, long ist_psf, long ied_psf,
                             struct psf_data **psf_s,
                             struct kemo_array_control *psf_a,
                             struct gl_strided_buffer *strided_buf);

long set_psf_map_to_buf(long ist_patch, long ist_psf, long ied_psf,
                        struct psf_data **psf_s,
                        struct kemo_array_control *psf_a,
                        struct gl_strided_buffer *strided_buf);

long add_num_psf_arrows(long ist_patch, long ist, long ied, int ncorner,
                        struct psf_data *psf_s, struct psf_menu_val *psf_m);
long set_psf_arrows_to_buf(long ist_patch, long ist, long ied,
                           int ncorner, struct psf_data *psf_s, struct psf_menu_val *psf_m,
                           struct gl_strided_buffer *strided_buf);
#endif
