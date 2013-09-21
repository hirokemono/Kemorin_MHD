/*
 *  set_kemoviewer_ucd_data.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/02.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#ifndef  SET_KEMOVIEWER_UCD_DATA_
#define  SET_KEMOVIEWER_UCD_DATA_

#include "m_psf_data_4_viewer_c.h"
#include "m_kemoviewer_menu.h"
#include "m_gl_transfer_matrix.h"
#include "read_data_4_kemoviewer.h"
#include "psf_data_array_manager.h"
#include "skip_comment_c.h"

/* prototypes */

int kemoview_open_data(const char *file_name, struct viewer_mesh *mesh_d, struct mesh_menu_val *mesh_m, 
					  struct kemo_array_control *psf_a, struct psf_data **psf_d, struct psf_menu_val **psf_m, 
					  struct psf_data *fline_d, struct fline_menu_val *fline_m, 
					  struct psf_data *ucd_tmp, struct ucd_file_menu_val *ucd_m,
					  struct view_element *view);
#endif
