/*
 *  m_kemoview_gl_VAOs.h
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#ifndef M_KEMOVIEW_GL_VAOS_
#define M_KEMOVIEW_GL_VAOS_

#include <stdlib.h>

#include "glsl.h"
#include "vartex_array_object_gl.h"

#include "m_kemoviewer_data.h"
#include "m_kemoview_gl_VAOs.h"
#include "drawGL_by_VAO.h"
#include "drawcube_gl.h"
#include "draw_PSF_patches_by_VAO.h"
#include "m_kemoview_object_buffers.h"


struct kemoview_VAOs{
    struct VAO_ids *cube_VAO;
    struct VAO_ids *msg_VAO;
    
    struct VAO_ids **mesh_solid_VAO;
    struct VAO_ids *mesh_trans_VAO;
    
    struct VAO_ids **fline_VAO;

    struct VAO_ids **psf_solid_index_VAO;
    struct VAO_ids **psf_trans_index_VAO;
    struct VAO_ids *map_index_VAO;

    struct VAO_ids **psf_solid_VAO;
    struct VAO_ids **psf_trans_VAO;
    struct VAO_ids *psf_liness_VAO;

    struct VAO_ids *axis_VAO;
    struct VAO_ids *grid_line_VAO;
    struct VAO_ids *grid_tube_VAO;
    struct VAO_ids **cbar_VAO;
    struct VAO_ids *time_VAO;
    
    struct VAO_ids **map_VAO;
    
    struct VAO_ids *screen_VAO;
    struct VAO_ids **screen_FBO;
};

/* prototypes */

struct kemoview_VAOs * init_kemoview_VAOs(void);
void assign_kemoview_VAOs(struct kemoview_VAOs *kemo_VAOs);
void clear_kemoview_VAOs(struct kemoview_VAOs *kemo_VAOs);
void dealloc_kemoview_VAOs(struct kemoview_VAOs *kemo_VAOs);

void set_transparent_objects_to_VAO(struct kemoview_buffers *kemo_buffers,
                                    struct kemoview_VAOs *kemo_VAOs);
void set_draw_objects_to_VAO(struct kemoview_psf *kemo_psf,
                             struct view_element *view_s,
                             struct kemoview_buffers *kemo_buffers,
                             struct kemoview_VAOs *kemo_VAOs,
                             struct kemoview_shaders *kemo_shaders);
 
#endif /*  M_KEMOVIEW_GL_VAOS_  */
