/*
  glsl.c
*/
#include <stdio.h>
#include <stdlib.h>

#include "glsl.h"

char *glsl_version = "#version 330 \n";

/*
** Init GLSL
*/
int glslInit(void)
{
	int error = 0;
	return error;
}


char *phong_colormap_vertex_shader(void){
    long len_colormap_rainbow =       strlen(load_colormap_rainbow_frag());
    long len_colormap_grayscale =     strlen(load_colormap_grayscale_frag());
    long len_colormap_sym_grayscale = strlen(load_colormap_sym_grayscale_frag());
    long len_colormap_space =         strlen(load_colormap_space_frag());
    long len_colormap_red_blue =      strlen(load_colormap_red_blue_frag());
    long len_colormap_orange_cyan =   strlen(load_colormap_orange_cyan_frag());
    long len_colormap_molten_metal =  strlen(load_colormap_molten_metal_frag());

    long len_color_normalize = strlen(load_color_normalize_frag());
    long len_colormap_select = strlen(load_colormap_select_frag());

    long len_phong_w_colormap = strlen(load_phong_w_colormap_vert());

    long len_vertex_shader = 0;
    len_vertex_shader += len_phong_w_colormap;
    len_vertex_shader += len_colormap_select;
    len_vertex_shader += len_color_normalize;
    len_vertex_shader += len_colormap_molten_metal;
    len_vertex_shader += len_colormap_orange_cyan;
    len_vertex_shader += len_colormap_red_blue;
    len_vertex_shader += len_colormap_space;
    len_vertex_shader += len_colormap_sym_grayscale;
    len_vertex_shader += len_colormap_grayscale;
    len_vertex_shader += len_colormap_rainbow;
    
    char *vertex_shader = alloc_string(len_vertex_shader);
    append_text_c(load_colormap_rainbow_frag(), vertex_shader);
    append_text_c(load_colormap_grayscale_frag(), vertex_shader);
    append_text_c(load_colormap_sym_grayscale_frag(), vertex_shader);
    append_text_c(load_colormap_space_frag(), vertex_shader);
    append_text_c(load_colormap_red_blue_frag(), vertex_shader);
    append_text_c(load_colormap_orange_cyan_frag(), vertex_shader);
    append_text_c(load_colormap_molten_metal_frag(), vertex_shader);
    
    append_text_c(load_color_normalize_frag(), vertex_shader);
    append_text_c(load_colormap_select_frag(), vertex_shader);
    
    append_text_c(load_phong_w_colormap_vert(), vertex_shader);
    return vertex_shader;
}

/*
** Read shader program from source
*/
static int readShaderSource(GLuint shader, const char *file)
{
	FILE *fp;
	const GLchar *source;
	GLsizei length;
	int ret;
	
	printf("file: %s\n", file);
	/* Open file */
	fp = fopen(file, "rb");
	if (fp == NULL) {
		perror(file);
		return -1;
	}
	
	/* Get file size */
	fseek(fp, 0L, SEEK_END);
	length = (GLsizei) ftell(fp);
	
	/* Allocate memory for source */
	source = (GLchar *)malloc(length);
	if (source == NULL) {
		fprintf(stderr, "Could not allocate read buffer.\n");
		return -1;
	}
	
	/* Read source file */
	fseek(fp, 0L, SEEK_SET);
	ret = fread((void *)source, 1, length, fp) != (size_t)length;
	fclose(fp);
	
	/* Send sheader source to GL */
	if (ret)
	fprintf(stderr, "Could not read file: %s.\n", file);
	else
	glShaderSource(shader, 1, &source, &length);
	
	/* Release memory for source */
	free((void *)source);
	
	return ret;
}

/*
** Display shader info
*/
static void printShaderInfoLog(GLuint shader)
{
	GLsizei bufSize;
	
	glGetShaderiv(shader, GL_INFO_LOG_LENGTH , &bufSize);
	
	if (bufSize > 1) {
		GLchar *infoLog;
		
		infoLog = (GLchar *)malloc(bufSize);
		if (infoLog != NULL) {
			GLsizei length;
			
			glGetShaderInfoLog(shader, bufSize, &length, infoLog);
			fprintf(stderr, "InfoLog:\n%s\n\n", infoLog);
			free(infoLog);
		}
		else
		fprintf(stderr, "Could not allocate InfoLog buffer.\n");
	}
	return;
}

/*
** Display shader program info
*/
static void printProgramInfoLog(GLuint program)
{
	GLsizei bufSize;
	
	glGetProgramiv(program, GL_INFO_LOG_LENGTH , &bufSize);
	
	if (bufSize > 1) {
		GLchar *infoLog;
		
		infoLog = (GLchar *)malloc(bufSize);
		if (infoLog != NULL) {
			GLsizei length;
			
			glGetProgramInfoLog(program, bufSize, &length, infoLog);
			fprintf(stderr, "InfoLog:\n%s\n\n", infoLog);
			free(infoLog);
		}
		else
		fprintf(stderr, "Could not allocate InfoLog buffer.\n");
	}
	return;
}

/* compile and link shader programs */

static void CompileLinkShader(struct shader_ids *shader)
{
	/* Valuable to get results of compile and link */
	GLint compiled, linked;
	
	/* Compile Vertex shader */
	glCompileShader(shader->vertexID);
	glGetShaderiv(shader->vertexID, GL_COMPILE_STATUS, &compiled);
	printShaderInfoLog(shader->vertexID);
	if (compiled == GL_FALSE) {
		fprintf(stderr, "Compile error in vertex shader.\n");
		exit(1);
	}
	
	/* Compile fragment shader */
	glCompileShader(shader->fragmentID);
	glGetShaderiv(shader->fragmentID, GL_COMPILE_STATUS, &compiled);
	printShaderInfoLog(shader->fragmentID);
	if (compiled == GL_FALSE) {
		fprintf(stderr, "Compile error in fragment shader.\n");
		exit(1);
	}
	
	/* Craate shader program */
	shader->programId = glCreateProgram();
	
  /* assine shader object to program */
	glAttachShader(shader->programId, shader->vertexID);
	glAttachShader(shader->programId, shader->fragmentID);
	
	/* Link shader progream */
	glLinkProgram(shader->programId);
	glGetProgramiv(shader->programId, GL_LINK_STATUS, &linked);
	printProgramInfoLog(shader->programId);
	if (linked == GL_FALSE) {
		fprintf(stderr, "Link error.\n");
		exit(1);
	}
	
	return;
}

/* compile and link shader from file */

void LoadShaderFromFile(struct shader_ids *shader,
			const char *fname_vertex, const char *fname_fragment)
{
	/* Make shader object */
	shader->vertexID = glCreateShader(GL_VERTEX_SHADER);
	shader->fragmentID = glCreateShader(GL_FRAGMENT_SHADER);
	
	/* reader shader source */
	if (readShaderSource(shader->vertexID, fname_vertex)) exit(1);
	if (readShaderSource(shader->fragmentID, fname_fragment)) exit(1);
	
	/* Compile and link  shader */
	CompileLinkShader(shader);
	
	/* Release shader object */
	glDeleteShader(shader->vertexID);
	glDeleteShader(shader->fragmentID);
	
	return;
}

void LoadShaderFromStrings(struct shader_ids *shader,
			const GLchar *text_vertex, const GLchar *text_fragment)
{
    long len_version = strlen(glsl_version);
    long len_vertex =   strlen(text_vertex) + len_version;
    long len_fragment = strlen(text_fragment) + len_version;

    char *vertex_shader =   alloc_string(len_vertex+len_version);
    append_text_c(glsl_version, vertex_shader);
    append_text_c(text_vertex, vertex_shader);

    char *fragment_shader = alloc_string(len_fragment+len_version);
    append_text_c(glsl_version, fragment_shader);
    append_text_c(text_fragment, fragment_shader);
/*
    printf("VERTEX: \n%s\n\n", vertex_shader);
    printf("FRAGMENT: \n%s\n\n", fragment_shader);
 */
    /* Make shader object */
	shader->vertexID = glCreateShader(GL_VERTEX_SHADER);
	shader->fragmentID = glCreateShader(GL_FRAGMENT_SHADER);
	
	/* reader shader source */
	glShaderSource(shader->vertexID, 1, &vertex_shader, 0);
	glShaderSource(shader->fragmentID, 1, &fragment_shader, 0);
	
	/* Compile and link  shader */
	CompileLinkShader(shader);
	
	/* Release shader object */
	glDeleteShader(shader->vertexID);
	glDeleteShader(shader->fragmentID);
	
	return;
}

void destory_shaders(struct shader_ids *shader)
{
	GLenum ierr_gl;
	
	glUseProgram(0);
	
	glDetachShader(shader->programId, shader->fragmentID);
	glDetachShader(shader->programId, shader->vertexID);
	
	glDeleteShader(shader->fragmentID);
	glDeleteShader(shader->vertexID);
	
	glDeleteProgram(shader->programId);
	
	ierr_gl = glGetError();
	
	/*
	if (ierr_gl != GL_NO_ERROR)
	{
      fprintf(
	      stderr,
	      "ERROR: Failed to destroy the shaders: %s \n",
	      gluErrorString(ierr_gl)
	      );

      exit(-1);
    }
	 */
}

void transfer_matrix_to_GL(struct shader_ids *Shader, struct transfer_matrices *matrices){
    int modelMatLocation =   glGetUniformLocation(Shader->programId, "modelViewMat");
    int projectMatLocation = glGetUniformLocation(Shader->programId, "projectionMat");
    int normalMatLocation =  glGetUniformLocation(Shader->programId, "modelNormalMat");
    
	glUniformMatrix4fv(modelMatLocation,   1, GL_FALSE, matrices->model);
	glUniformMatrix4fv(projectMatLocation, 1, GL_FALSE, matrices->proj);
	glUniformMatrix4fv(normalMatLocation,  1, GL_FALSE, matrices->nrmat);
};

struct gl_transfer_matrices * dup_transfer_matrices_for_gl(struct transfer_matrices *matrices)
{
    int i;
    struct gl_transfer_matrices *glmat;
    if((glmat = (struct gl_transfer_matrices *) malloc(sizeof(struct gl_transfer_matrices))) == NULL){
        printf("malloc error for gl_transfer_matrices\n");
        exit(0);
    };
    
    for(i=0;i<16;i++) {
        glmat->model[i] = (GLfloat) matrices->model[i];
        glmat->nrmat[i] = (GLfloat) matrices->nrmat[i];
        glmat->proj[i] =  (GLfloat) matrices->proj[i];
    };
    return glmat;
};

void map_matrix_to_GLSL(struct shader_ids *Shader, struct transfer_matrices *matrices){
    struct gl_transfer_matrices *glmat = dup_transfer_matrices_for_gl(matrices);
	
    int modelMatLocation =   glGetUniformLocation(Shader->programId, "modelViewMat");
    int projectMatLocation = glGetUniformLocation(Shader->programId, "projectionMat");
    int normalMatLocation =  glGetUniformLocation(Shader->programId, "modelNormalMat");
    
	glUniformMatrix4fv(modelMatLocation,   1, GL_FALSE, glmat->model);
	glUniformMatrix4fv(projectMatLocation, 1, GL_FALSE, glmat->proj);
	glUniformMatrix4fv(normalMatLocation,  1, GL_FALSE, glmat->nrmat);
    free(glmat);
    return;
};

void identity_matrix_to_shader(struct shader_ids *Shader){
	GLfloat model[16], proj[16], nrmat[16];
	int i;

/*	glUseProgram(Shader->programId);*/
	
	for(i=0;i<16;i++) {
        model[i] = 0.0;
	    proj[i] =  0.0;
        nrmat[i] = 0.0;
    };
    for(i=0;i<4;i++) {
        model[5*i] = 1.0;
        proj[5*i] =  1.0;
        nrmat[5*i] = 1.0;
    };
	
    int modelMatLocation =   glGetUniformLocation(Shader->programId, "modelViewMat");
    int projectMatLocation = glGetUniformLocation(Shader->programId, "projectionMat");
    int normalMatLocation =  glGetUniformLocation(Shader->programId, "modelNormalMat");
    
	glUniformMatrix4fv(modelMatLocation,   1, GL_FALSE, model);
	glUniformMatrix4fv(projectMatLocation, 1, GL_FALSE, proj);
	glUniformMatrix4fv(normalMatLocation,  1, GL_FALSE, nrmat);
};

struct shader_ids * init_shader_ids(void){
    struct shader_ids *s_id;
    if((s_id = (struct shader_ids *) malloc(sizeof(struct shader_ids))) == NULL){
        printf("malloc error in shader_ids \n");
        exit(0);
    };
    return s_id;
};

struct kemoview_shaders * init_kemoview_shaders(void){
	struct kemoview_shaders *sds;
	if((sds = (struct kemoview_shaders *) malloc(sizeof(struct kemoview_shaders))) == NULL){
		printf("malloc error in shaders \n");
		exit(0);
	};
	
	sds->gouraud = init_shader_ids();
	sds->menu =    init_shader_ids();
	sds->phong =   init_shader_ids();
    sds->phong_w_cmap =    init_shader_ids();
	sds->phong_texure =    init_shader_ids();
	sds->phong_1color =    init_shader_ids();
    sds->simple_texure =   init_shader_ids();
    sds->simple =          init_shader_ids();
	sds->anaglyph_texure = init_shader_ids();
	return sds;
};

void dealloc_kemoview_shaders(struct kemoview_shaders *sds){
    destory_shaders(sds->anaglyph_texure);
	destory_shaders(sds->simple_texure);
	destory_shaders(sds->phong_1color);
	destory_shaders(sds->phong_texure);
    destory_shaders(sds->phong_w_cmap);
	destory_shaders(sds->phong);
    destory_shaders(sds->gouraud);
	destory_shaders(sds->menu);
	destory_shaders(sds->simple);
		
	free(sds->gouraud);
	free(sds->menu);
	free(sds->phong);
	free(sds->simple);
	
	free(sds);
	return;
};


void set_phong_light_list(struct shader_ids *phong, struct phong_lights *lights){
	int i;
	int id_numLight;
	int id_lightPosition[10];
	
	int id_MaterialAmbient, id_MaterialDiffuse;
	int id_MaterialSpecular, id_MaterialShiness;
	
	id_numLight = glGetUniformLocation(phong->programId, "num_lights");
	id_lightPosition[0] = glGetUniformLocation(phong->programId, "LightSource[0].position");
	id_lightPosition[1] = glGetUniformLocation(phong->programId, "LightSource[1].position");
	id_lightPosition[2] = glGetUniformLocation(phong->programId, "LightSource[2].position");
	id_lightPosition[3] = glGetUniformLocation(phong->programId, "LightSource[3].position");
	id_lightPosition[4] = glGetUniformLocation(phong->programId, "LightSource[4].position");
	id_lightPosition[5] = glGetUniformLocation(phong->programId, "LightSource[5].position");
	id_lightPosition[6] = glGetUniformLocation(phong->programId, "LightSource[6].position");
	id_lightPosition[7] = glGetUniformLocation(phong->programId, "LightSource[7].position");
	id_lightPosition[8] = glGetUniformLocation(phong->programId, "LightSource[8].position");
	id_lightPosition[9] = glGetUniformLocation(phong->programId, "LightSource[9].position");
	
	id_MaterialAmbient = glGetUniformLocation(phong->programId, "frontMaterial.ambient");
	id_MaterialDiffuse = glGetUniformLocation(phong->programId, "frontMaterial.diffuse");
	id_MaterialSpecular = glGetUniformLocation(phong->programId, "frontMaterial.specular");
	id_MaterialShiness = glGetUniformLocation(phong->programId, "frontMaterial.shininess");
	
	
	glUniform1i(id_numLight, lights->n_light_point);
	for(i=0;i<lights->n_light_point;i++){
		glUniform4fv(id_lightPosition[i], 1, &lights->light_xyz[4*i]);
	};
	
	glUniform4fv(id_MaterialAmbient,  1, lights->ambient);
	glUniform4fv(id_MaterialDiffuse,  1, lights->diffuse);
	glUniform4fv(id_MaterialSpecular, 1, lights->specular);
	glUniform1fv(id_MaterialShiness,  1, lights->shiness);
};

