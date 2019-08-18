
/* draw_map_4_PSF.h */

#ifndef DRAW_MAP_4_PSF_
#define DRAW_MAP_4_PSF_

#include "kemoviewer_param_c.h"
#include "m_kemoviewer_menu.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "init_gl_lighting_c.h"
#include "set_new_patch_4_map_c.h"
#include "icosahedron_c.h"
#include "coordinate_converter_c.h"
#include "set_color_code_on_nodes.h"
#include "draw_isolines_4_map.h"
#include "draw_coastline.h"
#include "set_PSF_patches_to_buf.h"


/* prptotypes */

void draw_map_patch_VAO(int shading_mode, int ist_psf, int ied_psf, 
			struct psf_data **psf_s, struct kemo_array_control *psf_a, const GLdouble *orthogonal, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf);

int draw_map_objects_VAO(struct psf_data **psf_s, struct mesh_menu_val *mesh_m,
			struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
			struct view_element *view_s, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf, struct buffer_for_gl *gl_buf);
#endif
