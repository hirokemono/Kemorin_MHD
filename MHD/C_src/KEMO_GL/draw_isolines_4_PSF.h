/*
// draw_isolines_4_PSF.h
*/

#ifndef DRAW_ISOLINE_4_PSF_
#define DRAW_ISOLINE_4_PSF_

#include "gl2ps.h"

#include "m_kemoviewer_menu.h"
#include "m_psf_data_4_viewer_c.h"
#include "vartex_array_object_gl.h"
#include "rainbow_color_code_c.h"
#include "find_isoline_on_patch_c.h"
#include "set_new_patch_4_map_c.h"

/* prototypes */

double cal_isoline_value(int j, struct psf_menu_val *psf_m);
void find_start_positive_lines(struct psf_menu_val *psf_m);

void draw_PSF_isoline(struct psf_data *psf_s, struct psf_menu_val *psf_m,
					  struct buffer_for_gl *gl_buf, int iflag_retina, int iflag_write_ps);

#endif
