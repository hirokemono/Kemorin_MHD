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
#include "m_gl_transfer_matrix.h"
#include "rainbow_color_code_c.h"
#include "find_isoline_on_patch_c.h"
#include "set_new_patch_4_map_c.h"
#include "draw_isolines_4_PSF.h"

/* prototypes */

void draw_map_PSF_isoline(struct psf_data *psf_s, struct psf_menu_val *psf_m,
						  struct buffer_for_gl *gl_buf);

#endif
