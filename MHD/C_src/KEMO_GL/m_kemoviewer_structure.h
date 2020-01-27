
/* m_kemoviewer_structure.h*/

#ifndef M_KEMOVIEWER_STRACTURE_
#define M_KEMOVIEWER_STRACTURE_

#include "kemoviewer.h"
#include "kemoviewer_base.h"
#include "m_kemoviewer_data.h"
#include "m_surface_mesh_4_viewer_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_kemoview_psf.h"
#include "m_kemoview_fline.h"
#include "m_kemoview_mesh.h"
#include "m_colorbar_work.h"
#include "m_phong_light_table_c.h"
#include "m_gl_transfer_matrix.h"
#include "vartex_array_object_gl.h"
#include "read_data_4_kemoviewer.h"
#include "set_rgba_table_c.h"
#include "rainbow_color_code_c.h"
#include "psf_data_array_manager.h"
#include "modify_stereo_view_gl.h"
#include "set_kemoviewer_ucd_data.h"
#include "skip_comment_c.h"
#include "write_modelview_matrix.h"
#include "rotate_animation.h"
#include "set_each_psf_parameters.h"
#include "set_each_fline_parameters.h"
#include "set_texture_4_psf.h"
#include "numbers_to_bin_c.h"

#include "glsl.h"
#include "shaders.h"


#include "kemoviewer.h"

#ifdef PNG_OUTPUT
    #include "write_gl_window_to_file.h"
    #include "set_psf_texture_by_png.h"
#endif

#endif
