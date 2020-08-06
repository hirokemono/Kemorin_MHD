/*
  glsl.c
*/
#include <stdio.h>
#include <stdlib.h>

#include "glsl.h"


/*
** Init GLSL
*/
int glslInit(void)
{
	int error = 0;
	return error;
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
	/* Make shader object */
	shader->vertexID = glCreateShader(GL_VERTEX_SHADER);
	shader->fragmentID = glCreateShader(GL_FRAGMENT_SHADER);
	
	/* reader shader source */
	glShaderSource(shader->vertexID, 1, &text_vertex, 0);
	glShaderSource(shader->fragmentID, 1, &text_fragment, 0);
	
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


void transfer_matrix_to_shader(struct shader_ids *Shader, struct view_element *view_s){
	double a_inv[16];
	GLfloat model[16], proj[16], nrmat[9];
	int i;
	
	
	int modelMatLocation =   glGetUniformLocation(Shader->programId, "modelViewMat");
	int projectMatLocation = glGetUniformLocation(Shader->programId, "projectionMat");
	int normalMatLocation = glGetUniformLocation(Shader->programId, "modelNormalMat");
	
	cal_inverse_44_matrix_c(view_s->mat_object_2_eye, a_inv);
	for(i=0;i<16;i++) {model[i] = (GLfloat) view_s->mat_object_2_eye[i];};
	for(i=0;i<16;i++) {proj[i] = (GLfloat) view_s->mat_eye_2_clip[i];};
	for(i=0;i<3;i++) {
		nrmat[3*i  ] = (GLfloat) a_inv[  i];
		nrmat[3*i+1] = (GLfloat) a_inv[4+i];
		nrmat[3*i+2] = (GLfloat) a_inv[8+i];
	};
	
	glUniformMatrix4fv(modelMatLocation, 1,   GL_FALSE, model);
	glUniformMatrix4fv(projectMatLocation, 1, GL_FALSE, proj);
	glUniformMatrix3fv(normalMatLocation, 1, GL_FALSE, nrmat);
};

void map_matrix_to_shader(struct shader_ids *Shader, const double *orthogonal){
	GLfloat model[16], proj[16], nrmat[9];
	int i;
	
	
	int modelMatLocation =   glGetUniformLocation(Shader->programId, "modelViewMat");
	int projectMatLocation = glGetUniformLocation(Shader->programId, "projectionMat");
	int normalMatLocation = glGetUniformLocation(Shader->programId, "modelNormalMat");
	
	for(i=0;i<16;i++) {model[i] = 0.0;};
	for(i=0;i<9;i++)  {nrmat[i] = 0.0;};
	for(i=0;i<4;i++) {model[5*i] = 1.0;};
	for(i=0;i<3;i++) {nrmat[4*i] = 1.0;};
	for(i=0;i<16;i++) {proj[i] = (GLfloat) orthogonal[i];};
	
	glUniformMatrix4fv(modelMatLocation, 1, GL_FALSE, model);
	glUniformMatrix4fv(projectMatLocation, 1, GL_FALSE, proj);
	glUniformMatrix3fv(normalMatLocation, 1, GL_FALSE, nrmat);
};

void identity_matrix_to_shader(struct shader_ids *Shader){
	GLfloat model[16], proj[16], nrmat[9];
	int i;
	
/*	glUseProgram(Shader->programId);*/
	
	int modelMatLocation =   glGetUniformLocation(Shader->programId, "modelViewMat");
	int projectMatLocation = glGetUniformLocation(Shader->programId, "projectionMat");
	int normalMatLocation = glGetUniformLocation(Shader->programId, "modelNormalMat");
	
	for(i=0;i<16;i++) {model[i] = 0.0;};
	for(i=0;i<16;i++) {proj[i] =  0.0;};
	for(i=0;i<9;i++)  {nrmat[i] = 0.0;};
	for(i=0;i<4;i++) {model[5*i] = 1.0;};
	for(i=0;i<4;i++) {proj[5*i] =  1.0;};
	for(i=0;i<3;i++) {nrmat[4*i] = 1.0;};
	
	glUniformMatrix4fv(modelMatLocation, 1,   GL_FALSE, model);
	glUniformMatrix4fv(projectMatLocation, 1, GL_FALSE, proj);
	glUniformMatrix3fv(normalMatLocation, 1, GL_FALSE, nrmat);
};

static struct shader_ids * init_shader_ids(){
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
	sds->menu = init_shader_ids();
	sds->phong = init_shader_ids();
	sds->phong_texure = init_shader_ids();
	sds->phong_1color = init_shader_ids();
	sds->simple_texure = init_shader_ids();
	sds->simple = init_shader_ids();
	
	if((sds->lights = (struct phong_lights *) malloc(sizeof(struct phong_lights))) == NULL){
		printf("malloc error in phong_lights \n");
		exit(0);
	};
	
	
	/*
	LoadShaderFromStrings(sds->gouraud, load_gouraud_vert(), load_gouraud_frag());
	LoadShaderFromStrings(sds->phong, load_phong_vert(), load_phong_frag());
	LoadShaderFromStrings(sds->menu, load_menu_vert(), load_menu_frag());
	LoadShaderFromStrings(sds->simple, load_simple_vert(), load_simple_frag());
	*/
	return sds;
};

void dealloc_kemoview_shaders(struct kemoview_shaders *sds){
	destory_shaders(sds->simple_texure);
	destory_shaders(sds->phong_1color);
	destory_shaders(sds->phong_texure);
	destory_shaders(sds->gouraud);
	destory_shaders(sds->phong);
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

