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

#include "kemoviewer.h"
#include "m_kemoview_psf.h"
#include "m_kemoview_fline.h"
#include "m_kemoview_tracer.h"
#include "m_kemoview_mesh.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_kemoview_mesh_menu.h"
#include "m_gl_transfer_matrix.h"
#include "kemoviewer_base.h"
#include "read_data_4_kemoviewer.h"
#include "psf_data_array_manager.h"
#include "skip_comment_c.h"

/* prototypes */

int set_data_format_flag(const char *file_name, char *file_head, char *file_ext);

int kemoviewer_open_data(struct kv_string *filename, 
                         struct kemoview_mesh *kemo_mesh, 
                         struct kemoview_mul_psf *kemo_mul_psf, 
                         struct kemoview_fline *kemo_fline,
                         struct kemoview_tracer *kemo_tracer,
                         struct psf_data *ucd_tmp,
                         struct view_element *view,
                         int *istep_file);

#endif
