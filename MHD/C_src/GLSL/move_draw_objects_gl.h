/*
 *  move_draw_objects_gl.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#ifndef MOVE_DRAW_OBJECTS_GL_
#define MOVE_DRAW_OBJECTS_GL_

#include <stdlib.h>

#include "glsl.h"
#include "vartex_array_object_gl.h"

#include "m_kemoviewer_data.h"
#include "m_kemoview_gl_VAOs.h"
#include "drawGL_by_VAO.h"
#include "drawcube_gl.h"
#include "draw_PSF_patches_by_VAO.h"
#include "m_kemoview_object_buffers.h"
#include "m_kemoview_gl_VAOs.h"


/* prototypes */
void get_gl_buffer_to_bmp(int num_x, int num_y, unsigned char *glimage);
void update_draw_objects_gl(struct kemoview_mul_psf *kemo_mul_psf, 
                            struct kemoview_fline *kemo_fline,
                            struct kemoview_tracer *kemo_tracer,
                            struct kemoview_mesh *kemo_mesh, 
                            struct view_element *view_s,
                            struct kemoview_buffers *kemo_buffers,
                            struct kemoview_VAOs *kemo_VAOs,
                            struct kemoview_shaders *kemo_shaders);

#endif
