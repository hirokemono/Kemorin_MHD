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
#include "m_kemoview_mesh_menu.h"
#include "m_kemoviewer_menu.h"
#include "m_gl_transfer_matrix.h"
#include "read_data_4_kemoviewer.h"
#include "psf_data_array_manager.h"
#include "skip_comment_c.h"

/* prototypes */

int set_data_format_flag(const char *file_name, char *file_head, char *file_ext);
int kemoviewer_open_data(struct kv_string *filename, struct viewer_mesh *mesh_d, struct mesh_menu_val *mesh_m, 
					  struct kemo_array_control *psf_a, struct psf_data **psf_d, struct psf_menu_val **psf_m, 
					  struct psf_data *fline_d, struct fline_menu_val *fline_m, 
					  struct psf_data *ucd_tmp, struct view_element *view);
#endif
