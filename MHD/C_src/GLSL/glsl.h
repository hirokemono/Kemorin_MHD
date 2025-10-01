/*
  glsl.h
*/

#ifndef GLSL__
#define GLSL__

#ifdef __APPLE__
#include<OpenGL/gl3.h>
#else
#include<GL/gl.h>
#endif

#include "m_gl_transfer_matrix.h"
#include "m_phong_light_table_c.h"
#include "m_transfer_matrices.h"
#include "shaders.h"
#include "invert_small_matrix_c.h"

struct shader_ids{
	GLuint programId;
	
	GLuint vertexID;
	GLuint fragmentID;
    
    char *vetex_text;
    char *fragment_text;
};

struct kemoview_shaders{
	struct shader_ids *gouraud;
	struct shader_ids *phong;
    struct shader_ids *phong_w_cmap;
	struct shader_ids *phong_texure;
	struct shader_ids *phong_1color;
	struct shader_ids *menu;
	struct shader_ids *simple_texure;
	struct shader_ids *simple;
    struct shader_ids *anaglyph_texure;
    
    GLuint texture_name;
};

struct gl_transfer_matrices{
    GLfloat model[16];
    GLfloat proj[16];
    GLfloat nrmat[16];
};

/* prptotypes */
int glslInit(void);
char *phong_colormap_vertex_shader(void);


void LoadShaderFromFile(struct shader_ids *shader,
			const char *fname_vertex, const char *fname_fragment);
void LoadShaderFromStrings(struct shader_ids *shader,
			const GLchar *text_vertex, const GLchar *text_fragment);
void destory_shaders(struct shader_ids *shader);

struct gl_transfer_matrices * dup_transfer_matrices_for_gl(struct transfer_matrices *matrices);

void transfer_matrix_to_GL(struct shader_ids *Shader, struct transfer_matrices *matrices);
void map_matrix_to_GLSL(struct shader_ids *Shader, struct transfer_matrices *matrices);
void identity_matrix_to_shader(struct shader_ids *Shader);

struct shader_ids * init_shader_ids(void);
struct kemoview_shaders * init_kemoview_shaders(void);
void dealloc_kemoview_shaders(struct kemoview_shaders *sds);

void set_phong_light_list(struct shader_ids *phong, struct phong_lights *lights);

#endif
