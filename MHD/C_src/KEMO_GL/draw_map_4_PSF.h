
/* draw_map_4_PSF.h */

#ifndef DRAW_MAP_4_PSF_
#define DRAW_MAP_4_PSF_

#include "kemoviewer_param_c.h"
#include "m_kemoview_mesh_menu.h"
#include "m_kemoview_psf_menu.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_vertex_buffer.h"
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

void set_map_PSF_isolines_buffer(struct psf_data **psf_s, struct psf_menu_val **psf_m,
                                 struct kemo_array_control *psf_a, struct view_element *view_s,
                                 struct gl_strided_buffer *mline_buf);
void set_map_patch_buffer(int ist_psf, int ied_psf, struct psf_data **psf_s,
                          struct psf_menu_val **psf_m,
                          struct kemo_array_control *psf_a,
                          struct gl_strided_buffer *map_buf);
#endif
