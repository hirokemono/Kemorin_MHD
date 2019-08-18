/*
// draw_isolines_4_PSF.h
*/

#ifndef DRAW_ISOLINE_4_PSF_
#define DRAW_ISOLINE_4_PSF_

#include "m_kemoviewer_menu.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_gl_transfer_matrix.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"
#include "rainbow_color_code_c.h"
#include "find_isoline_on_patch_c.h"
#include "set_new_patch_4_map_c.h"
#include "set_PSF_isolines_to_buf.h"

/* prototypes */

void draw_PSF_isoline_VAO(struct psf_data *psf_s, struct psf_menu_val *psf_m,
			struct view_element *view_s, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf);

#endif
