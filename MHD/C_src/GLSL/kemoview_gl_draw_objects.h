/*
 *  kemoview_gl_draw_objects.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#ifndef KEMOVIEW_GL_DRAW_OBJECTS_
#define KEMOVIEW_GL_DRAW_OBJECTS_

#include <stdlib.h>

#include "glsl.h"
#include "vartex_array_object_gl.h"

#include "m_kemoviewer_data.h"
#include "m_vertex_buffer.h"
#include "m_kemoview_gl_VAOs.h"
#include "m_kemoview_object_buffers.h"
#include "m_kemoview_gl_VAOs.h"
#include "drawGL_by_VAO.h"
#include "drawcube_gl.h"
#include "draw_PSF_patches_by_VAO.h"
#include "move_draw_objects_gl.h"


/* prototypes */

void update_draw_objects_gl3(struct kemoviewer_type *kemoview,
                             struct kemoview_VAOs *kemo_VAOs,
                             struct kemoview_shaders *kemo_shaders);

struct gl_texure_image * draw_objects_to_rgb_gl(struct kemoviewer_type *kemoview,
                                                struct kemoview_VAOs *kemo_VAOs,
                                                struct kemoview_shaders *kemo_shaders);

struct gl_texure_image * draw_anaglyph_to_rgb_gl(struct kemoviewer_type *kemoview,
                                                 struct kemoview_VAOs *kemo_VAOs,
                                                 struct kemoview_shaders *kemo_shaders);

void select_modify_anaglyph(struct kemoviewer_type *kemoview,
                            struct kemoview_VAOs *kemo_VAOs,
                            struct kemoview_shaders *kemo_shaders);

#endif /* KEMOVIEW_GL_DRAW_OBJECTS_ */

