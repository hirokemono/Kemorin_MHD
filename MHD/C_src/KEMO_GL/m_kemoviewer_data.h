/*
//  m_kemoviewer_data.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/

#ifndef M_KEMOVIEWER_DATA_
#define M_KEMOVIEWER_DATA_

#include "kemoviewer.h"
#include "kemoviewer_base.h"
#include "m_kemoview_psf.h"
#include "m_kemoview_fline.h"
#include "m_kemoview_mesh.h"
#include "m_gl_transfer_matrix.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"
#include "shaders.h"

struct kemoviewer_type{
	int window_ID;
	
	struct kemoview_mesh      *kemo_mesh;
	struct kemoview_psf       *kemo_psf;
    struct kemoview_fline     *kemo_fline;
    
    struct view_element       *view_s;
	struct kemoview_shaders   *kemo_shaders;
	struct kemoview_VAOs      *kemo_VAOs;
	struct VAO_ids            *menu_VAO;
	
	struct psf_data           *psf_ucd_tmp;
	
	struct kemoviewer_type    *next;
};

struct mul_kemoviewer_type{
	int num_window;
	int id_current;
	struct kemoviewer_type   **kemo_mul;
};

/*  prototype */

#endif

