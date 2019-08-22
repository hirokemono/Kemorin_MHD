
/* draw_patches_4_PSF.h */

#ifndef DRAW_PATCHES_4_PSF_
#define DRAW_PATCHES_4_PSF_

#include "kemoviewer_param_c.h"
#include "m_kemoview_psf_menu.h"
#include "m_gl_transfer_matrix.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"
#include "m_psf_data_4_viewer_c.h"
#include "init_gl_lighting_c.h"
#include "set_new_patch_4_map_c.h"
#include "icosahedron_c.h"
#include "coordinate_converter_c.h"
#include "set_color_code_on_nodes.h"
#include "set_PSF_patches_to_buf.h"
#include "set_PSF_isolines_to_buf.h"


/* prptotypes */

void release_PSF_texture_from_gl(struct psf_menu_val *psf_m);

int check_draw_psf(struct kemo_array_control *psf_a);
void set_PSF_solid_objects_VAO(int shading_mode, 
			struct psf_data **psf_s, struct psf_menu_val **psf_m,
			struct kemo_array_control *psf_a, struct VAO_ids **psf_solid_VAO);
void draw_PSF_solid_objects_VAO(int shading_mode,
			struct psf_data **psf_s, struct psf_menu_val **psf_m,
			struct kemo_array_control *psf_a, struct view_element *view_s, 
			struct VAO_ids **psf_solid_VAO, struct kemoview_shaders *kemo_shaders);
void draw_PSF_isolines_VAO(int shading_mode, struct view_element *view_s, 
			struct VAO_ids **psf_solid_VAO, struct kemoview_shaders *kemo_shaders);
void draw_PSF_trans_objects_VAO(int shading_mode, 
			struct psf_data **psf_s, struct psf_menu_val **psf_m,
			struct kemo_array_control *psf_a, struct view_element *view_s, 
			struct VAO_ids **psf_trans_VAO, struct kemoview_shaders *kemo_shaders);
#endif
