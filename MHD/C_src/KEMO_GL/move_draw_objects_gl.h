
/* move_draw_objects_gl.h */

#ifndef MOVE_DRAW_OBJECT_GL_
#define MOVE_DRAW_OBJECT_GL_

#include <stdlib.h>

#include "m_kemoviewer_data.h"
#include "vartex_array_object_gl.h"
#include "drawGL_by_VAO.h"
#include "draw_colorbar_gl.h"
#include "draw_messages_gl.h"
#include "draw_fieldlines.h"
#include "draw_coastline.h"
#include "drawcube_gl.h"
#include "draw_patches_4_PSF.h"
#include "draw_map_4_PSF.h"
#include "draw_patch_4_mesh_c.h"
#include "set_texture_4_psf.h"

struct kemoview_buffers{
    struct gl_strided_buffer *cube_buf;
    struct gl_index_buffer *cube_index_buf;
    
    struct gl_strided_buffer *PSF_solid_buf;
    struct gl_strided_buffer *PSF_trns_buf;
    struct gl_strided_buffer *PSF_stxur_buf;
    struct gl_strided_buffer *PSF_ttxur_buf;
    struct gl_strided_buffer *PSF_arrow_buf;
    struct gl_strided_buffer *PSF_isoline_buf;

    struct gl_strided_buffer *mesh_solid_buf;
    struct gl_strided_buffer *mesh_grid_buf;
    struct gl_strided_buffer *mesh_node_buf;
    struct gl_strided_buffer *mesh_trns_buf;

    int ncorner_axis;
    struct gl_strided_buffer *axis_buf;
    
    struct gl_strided_buffer *cbar_buf;
    struct gl_strided_buffer *min_buf;
    struct gl_strided_buffer *max_buf;
    struct gl_strided_buffer *zero_buf;
    struct gl_strided_buffer *time_buf;

    struct gl_strided_buffer *msg_buf;
};

struct kemoview_VAOs{
	struct VAO_ids *cube_VAO;
    struct VAO_ids *msg_VAO;
	
	struct VAO_ids **mesh_solid_VAO;
	struct VAO_ids *mesh_trans_VAO;

	struct VAO_ids **fline_VAO;
	struct VAO_ids **psf_solid_VAO;
	struct VAO_ids **psf_trans_VAO;

	struct VAO_ids **grid_VAO;
	struct VAO_ids **cbar_VAO;
    struct VAO_ids *time_VAO;

	struct VAO_ids **map_VAO;
};


/* prototypes */
struct kemoview_buffers * init_kemoview_buffers(void);
void dealloc_kemoview_buffers(struct kemoview_buffers *kemo_buffers);


struct kemoview_VAOs * init_kemoview_VAOs(void);
void assign_kemoview_VAOs(struct kemoview_VAOs *kemo_VAOs);
void clear_kemoview_VAOs(struct kemoview_VAOs *kemo_VAOs);
void dealloc_kemoview_VAOs(struct kemoview_VAOs *kemo_VAOs);

void get_gl_buffer_to_bmp(int num_x, int num_y, unsigned char *glimage);

void quick_draw_objects_gl3(struct kemoviewer_type *kemoview);
void update_draw_objects_gl3(struct kemoviewer_type *kemoview);
#endif
