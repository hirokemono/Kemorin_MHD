/*
  glsl.c
*/

#ifndef GLSL__
#define GLSL__

#include "kemoviewer.h"
#include "m_gl_transfer_matrix.h"
#include "m_phong_light_table_c.h"
#include "set_transfer_matrices.h"
#include "shaders.h"
#include "invert_small_matrix_c.h"

struct shader_ids{
	GLuint programId;
	
	GLuint vertexID;
	GLuint fragmentID;
};

struct kemoview_shaders{
	struct shader_ids *gouraud;
	struct shader_ids *phong;
	struct shader_ids *phong_texure;
	struct shader_ids *phong_1color;
	struct shader_ids *menu;
	struct shader_ids *simple_texure;
	struct shader_ids *simple;
	
	struct phong_lights *lights; 
};

struct gl_transfer_matrices{
    GLfloat model[16];
    GLfloat proj[16];
    GLfloat nrmat[9];
};

/* prptotypes */

int glslInit(void);
void LoadShaderFromFile(struct shader_ids *shader,
			const char *fname_vertex, const char *fname_fragment);
void LoadShaderFromStrings(struct shader_ids *shader,
			const GLchar *text_vertex, const GLchar *text_fragment);
void destory_shaders(struct shader_ids *shader);

struct gl_transfer_matrices * dup_transfer_matrices_for_gl(struct transfer_matrices *matrices);
void transfer_matrix_to_shader(struct shader_ids *Shader, struct view_element *view_s);
void map_matrix_to_GLSL(struct shader_ids *Shader, struct transfer_matrices *matrices);
void identity_matrix_to_shader(struct shader_ids *Shader);

struct kemoview_shaders * init_kemoview_shaders(void);
void dealloc_kemoview_shaders(struct kemoview_shaders *sds);

void set_phong_light_list(struct shader_ids *phong, struct phong_lights *lights);

#endif
