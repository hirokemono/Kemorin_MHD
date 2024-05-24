
/* move_draw_objects_gl.h */

#ifndef MOVE_DRAW_OBJECT_GL_
#define MOVE_DRAW_OBJECT_GL_

#include <stdlib.h>

#include "glsl.h"
#include "vartex_array_object_gl.h"

#include "m_kemoviewer_data.h"
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

struct kemoviewer_gl_type{
    struct kemoview_shaders   *kemo_shaders;
    struct kemoview_VAOs      *kemo_VAOs;
    struct VAO_ids            *menu_VAO;
};


/* prototypes */
struct kemoview_VAOs * init_kemoview_VAOs(void);
void assign_kemoview_VAOs(struct kemoview_VAOs *kemo_VAOs);
void clear_kemoview_VAOs(struct kemoview_VAOs *kemo_VAOs);
void dealloc_kemoview_VAOs(struct kemoview_VAOs *kemo_VAOs);

void get_gl_buffer_to_bmp(int num_x, int num_y, unsigned char *glimage);

void update_draw_objects_gl3(struct kemoviewer_type *kemoview,
                             struct kemoviewer_gl_type *kemo_gl);

void draw_objects_to_rgb_gl(struct kemoviewer_type *kemoview,
                            struct kemoviewer_gl_type *kemo_gl,
                            struct gl_texure_image *image);
void draw_anaglyph_to_rgb_gl(struct kemoviewer_type *kemoview,
                             struct kemoviewer_gl_type *kemo_gl,
                             struct gl_texure_image *anaglyph_image);

unsigned char * draw_objects_to_rgb_by_FBO(GLuint npix_xy[2],
                                           struct kemoviewer_type *kemoview,
                                           struct kemoviewer_gl_type *kemo_gl);
void select_modify_anaglyph(struct kemoviewer_type *kemoview,
                            struct kemoviewer_gl_type *kemo_gl);

#endif
