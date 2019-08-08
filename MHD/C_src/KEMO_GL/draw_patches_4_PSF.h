
/* draw_patches_4_PSF.h */

#ifndef DRAW_PATCHES_4_PSF_
#define DRAW_PATCHES_4_PSF_

#include "kemoviewer_param_c.h"
#include "m_kemoviewer_menu.h"
#include "vartex_array_object_gl.h"
#include "m_kemoviewer_menu.h"
#include "init_gl_lighting_c.h"
#include "set_new_patch_4_map_c.h"
#include "icosahedron_c.h"
#include "coordinate_converter_c.h"
#include "set_color_code_on_nodes.h"


/* prptotypes */

void draw_arrow_4_PSF(struct psf_data *psf_s, struct psf_menu_val *psf_m, struct buffer_for_gl *gl_bufs);

void draw_patch_4_PSF(int shading_mode, int ist_psf, int ied_psf, 
                      struct psf_data **psf_s, struct psf_menu_val **psf_m,
					  struct kemo_array_control *psf_a, struct buffer_for_gl *gl_buf);
void draw_patches_4_map(int shading_mode, int ist_psf, int ied_psf,
                        struct psf_data **psf_s, struct kemo_array_control *psf_a,
                        struct buffer_for_gl *gl_buf);

void draw_texure_4_PSF(int shading_mode, int ist_psf, int ied_psf, 
                        struct psf_data **psf_s, struct psf_menu_val **psf_m,
                        struct kemo_array_control *psf_a, struct buffer_for_gl *gl_buf);

#endif
