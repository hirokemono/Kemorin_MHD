
/* m_kemoviewer_structure.h*/

#ifndef M_KEMOVIEWER_STRACTURE_
#define M_KEMOVIEWER_STRACTURE_

#include "m_surface_mesh_4_viewer_c.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_kemoviewer_menu.h"
#include "m_gl_transfer_matrix.h"
#include "read_data_4_kemoviewer.h"
#include "set_rgba_table_c.h"
#include "rainbow_color_code_c.h"
#include "psf_data_array_manager.h"
#include "modify_stereo_view_gl.h"
#include "set_kemoviewer_ucd_data.h"
#include "skip_comment_c.h"
#include "write_modelview_matrix.h"
#include "draw_menu_bottun_gl.h"
#include "rotate_animation.h"
#include "set_each_psf_parameters.h"
#include "set_texture_4_psf.h"
#include "numbers_to_bin_c.h"
#include "gl_buffer_2_gl2ps.h"
#include "set_psf_texture_by_png.h"


#include "kemoviewer.h"

#ifdef PNG_OUTPUT
    #include "write_gl_window_to_file.h"
    #include "set_psf_texture_by_png.h"
#endif

#endif
