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
#include "m_kemoview_tracer.h"
#include "m_kemoview_mesh.h"
#include "m_psf_data_4_viewer_c.h"
#include "m_gl_transfer_matrix.h"
#include "m_kemoview_object_buffers.h"

struct kemoviewer_type{
	int window_ID;
    int image_format_id;
    int istep_evo;
	
	struct kemoview_mesh      *kemo_mesh;
	struct kemoview_mul_psf   *kemo_mul_psf;
    struct kemoview_fline     *kemo_fline;
    struct kemoview_tracer    *kemo_tracer;
    struct psf_data           *psf_ucd_tmp;

    struct view_element       *view_s;

    struct kemoview_buffers   *kemo_buffers;

    struct kemoviewer_type    *next;
};

struct mul_kemoviewer_type{
	int num_window;
	int id_current;
	struct kemoviewer_type   **kemo_mul;
};

/*  prototype */

void set_default_image_format_id(struct kemoviewer_type *kemo_sgl, int input);
int send_default_image_format_id(struct kemoviewer_type *kemo_sgl);

#endif

