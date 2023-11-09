
/* drawGL_by_VAO.h */

#include <math.h>

#include "kemoviewer.h"
#include "m_gl_transfer_matrix.h"
#include "vartex_array_object_gl.h"
#include "glsl.h"

#ifndef DRAWGL_BY_VAO_
#define DRAWGL_BY_VAO_

/* prototypes */

void drawgl_textured_patches_VAO(GLuint *texture_name, struct transfer_matrices *matrices, 
								 struct VAO_ids *VAO, struct kemoview_shaders *kemo_shaders);
void drawgl_patch_with_phong(struct transfer_matrices *matrices, struct VAO_ids *VAO,
                             struct kemoview_shaders *kemo_shaders);
void drawgl_elements_with_phong(struct transfer_matrices *matrices, struct VAO_ids *VAO, 
                                struct kemoview_shaders *kemo_shaders);
void drawgl_lines(struct transfer_matrices *matrices, struct VAO_ids *VAO,
                  struct kemoview_shaders *kemo_shaders);

void draw_map_objects_VAO(struct transfer_matrices *matrices, 
			struct VAO_ids **map_VAO, struct kemoview_shaders *kemo_shaders);

void draw_solid_mesh_VAO(int polygon_mode, struct view_element *view_s, 
			struct VAO_ids *mesh_solid_VAO, struct kemoview_shaders *kemo_shaders);
void draw_trans_mesh_VAO(struct view_element *view_s, 
			struct VAO_ids *mesh_trans_VAO, struct kemoview_shaders *kemo_shaders);

void draw_2D_box_patch_VAO(struct transfer_matrices *matrices, struct VAO_ids *VAO, 
						   struct kemoview_shaders *kemo_shaders);
void draw_textured_2D_box_VAO(GLuint texture_name, struct transfer_matrices *matrices,
							  struct VAO_ids *VAO, struct kemoview_shaders *kemo_shaders);

#endif
