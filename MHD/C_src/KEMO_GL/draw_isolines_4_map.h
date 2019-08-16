/*
 *  draw_isolines_4_map.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/16.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#ifndef DRAW_ISOLINE_4_MAP_
#define DRAW_ISOLINE_4_MAP_

#include "m_kemoviewer_menu.h"
#include "m_psf_data_4_viewer_c.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"
#include "rainbow_color_code_c.h"
#include "find_isoline_on_patch_c.h"
#include "set_new_patch_4_map_c.h"
#include "draw_isolines_4_PSF.h"

/* prototypes */

void draw_map_PSF_isolines_VAO(struct psf_data *psf_s, struct psf_menu_val *psf_m,
			int iflag_retina, const GLdouble *orthogonal, 
			struct VAO_ids *psf_VAO, struct kemoview_shaders *kemo_shaders, 
			struct gl_strided_buffer *psf_buf);
#endif
