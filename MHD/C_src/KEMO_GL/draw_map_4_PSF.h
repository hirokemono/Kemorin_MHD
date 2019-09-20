
/* draw_map_4_PSF.h */

#ifndef DRAW_MAP_4_PSF_
#define DRAW_MAP_4_PSF_

#include "kemoviewer_param_c.h"
#include "m_kemoview_mesh_menu.h"
#include "m_kemoview_psf_menu.h"
#include "m_psf_data_4_viewer_c.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"
#include "init_gl_lighting_c.h"
#include "set_new_patch_4_map_c.h"
#include "icosahedron_c.h"
#include "coordinate_converter_c.h"
#include "set_color_code_on_nodes.h"
#include "draw_coastline.h"
#include "rainbow_color_code_c.h"
#include "set_PSF_patches_to_buf.h"
#include "set_map_isolines_to_buf.h"
#include "modify_object_4_viewer_c.h"


/* prptotypes */

int check_draw_map(struct kemo_array_control *psf_a);
void set_map_objects_VAO(struct view_element *view_s, 
						 struct psf_data **psf_s, struct mesh_menu_val *mesh_m,
						 struct psf_menu_val **psf_m, struct kemo_array_control *psf_a,
						 struct VAO_ids **map_VAO);
#endif
