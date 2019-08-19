
/* move_draw_objects_gl.h */

#ifndef MOVE_DRAW_OBJECT_GL_
#define MOVE_DRAW_OBJECT_GL_
/* Size of window */ 

#include "m_kemoviewer_menu.h"
#include "m_gl_transfer_matrix.h"
#include "vartex_array_object_gl.h"
#include "draw_colorbar_gl.h"
#include "draw_fieldlines.h"
#include "draw_coastline.h"
#include "drawcube_gl.h"
#include "draw_patches_4_PSF.h"
#include "draw_map_4_PSF.h"
#include "draw_patch_4_mesh_c.h"


/* prototypes */ 
void get_gl_buffer_to_bmp(int num_x, int num_y, unsigned char *glimage);

void draw_objects(struct viewer_mesh *mesh_s, struct psf_data **psf_s, 
			struct psf_data *fline_s, struct mesh_menu_val *mesh_m,
			struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, 
			struct fline_menu_val *fline_m, struct view_element *view_s,
			struct gl_strided_buffer *strided_buf, struct VAO_ids *cube_VAO,
			struct VAO_ids *mesh_solid_VAO, struct VAO_ids *mesh_grid_VAO, 
			struct VAO_ids *mesh_node_VAO, struct VAO_ids *mesh_trans_VAO, 
			struct VAO_ids *psf_solid_VAO, struct VAO_ids *psf_trans_VAO, 
			struct VAO_ids *psf_texture_VAO, struct VAO_ids *psf_trans_tex_VAO, 
			struct kemoview_shaders *kemo_shaders);

void draw_objects_gl3(struct viewer_mesh *mesh_s, struct psf_data **psf_s, 
			struct psf_data *fline_s, struct mesh_menu_val *mesh_m,
			struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, 
			struct fline_menu_val *fline_m, struct view_element *view_s,
			struct gl_strided_buffer *strided_buf, struct VAO_ids *cube_VAO, 
			struct VAO_ids *mesh_solid_VAO, struct VAO_ids *mesh_grid_VAO, 
			struct VAO_ids *mesh_node_VAO, struct VAO_ids *mesh_trans_VAO, 
			struct VAO_ids *psf_solid_VAO, struct VAO_ids *psf_trans_VAO, 
			struct VAO_ids *psf_texture_VAO, struct VAO_ids *psf_trans_tex_VAO, 
			struct kemoview_shaders *kemo_shaders);
void update_draw_objects_gl3(struct viewer_mesh *mesh_s, struct psf_data **psf_s, 
			struct psf_data *fline_s, struct mesh_menu_val *mesh_m,
			struct psf_menu_val **psf_m, struct kemo_array_control *psf_a, 
			struct fline_menu_val *fline_m, struct view_element *view_s,
			struct gl_strided_buffer *strided_buf, struct VAO_ids *cube_VAO, 
			struct VAO_ids *mesh_solid_VAO, struct VAO_ids *mesh_grid_VAO, 
			struct VAO_ids *mesh_node_VAO, struct VAO_ids *mesh_trans_VAO, 
			struct VAO_ids *psf_solid_VAO, struct VAO_ids *psf_trans_VAO, 
			struct VAO_ids *psf_texture_VAO, struct VAO_ids *psf_trans_tex_VAO, 
			struct kemoview_shaders *kemo_shaders);

void draw_quad_gl3(struct view_element *view_s,
			struct VAO_ids *quad_VAO, struct kemoview_shaders *kemo_shaders);
#endif
