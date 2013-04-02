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
	
	/* Open file */
	fp = fopen(file, "rb");
	if (fp == NULL) {
		perror(file);
		return -1;
	}
	
	/* Get file size */
	fseek(fp, 0L, SEEK_END);
	length = ftell(fp);
	
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

/* compile and lint shader programs */

static void CompileLinkShader(GLuint *Gl2Program, 
			GLuint *VertShader, GLuint *FragShader)
{
	/* Valuable to get results of compile and link */
	GLint compiled, linked;
	
	/* Compile Vertex shader */
	glCompileShader(*VertShader);
	glGetShaderiv(*VertShader, GL_COMPILE_STATUS, &compiled);
	printShaderInfoLog(*VertShader);
	if (compiled == GL_FALSE) {
		fprintf(stderr, "Compile error in Phong vertex shader.\n");
		exit(1);
	}
	
	/* Compile fragment shader */
	glCompileShader(*FragShader);
	glGetShaderiv(*FragShader, GL_COMPILE_STATUS, &compiled);
	printShaderInfoLog(*FragShader);
	if (compiled == GL_FALSE) {
		fprintf(stderr, "Compile error in Phong fragment shader.\n");
		exit(1);
	}
	
	/* Craate shader program */
	*Gl2Program = glCreateProgram();
	
  /* assine shader object to program */
	glAttachShader(*Gl2Program, *VertShader);
	glAttachShader(*Gl2Program, *FragShader);
	
	/* Link shader progream */
	glLinkProgram(*Gl2Program);
	glGetProgramiv(*Gl2Program, GL_LINK_STATUS, &linked);
	printProgramInfoLog(*Gl2Program);
	if (linked == GL_FALSE) {
		fprintf(stderr, "Link error.\n");
		exit(1);
	}
	
	return;
}

/* compile and lint shader from file */

void LoadShaderFromFile(GLuint *Gl2Program,
			const char *fname_vertex, const char *fname_fragment)
{
	/* Shader object */
	static GLuint VertShader;
	static GLuint FragShader;
	
	/* Make shader object */
	VertShader = glCreateShader(GL_VERTEX_SHADER);
	FragShader = glCreateShader(GL_FRAGMENT_SHADER);
	
	/* reader shader source */
	if (readShaderSource(VertShader, fname_vertex)) exit(1);
	if (readShaderSource(FragShader, fname_fragment)) exit(1);
	
	/* Compile and link  shader */
	CompileLinkShader(Gl2Program, &VertShader, &FragShader);
	
	/* Release shader object */
	glDeleteShader(VertShader);
	glDeleteShader(FragShader);
	
	return;
}

void LoadShaderFromStrings(GLuint *Gl2Program,
			const GLchar *text_vertex, const GLchar *text_fragment)
{
	/* Shader object */
	static GLuint VertShader;
	static GLuint FragShader;
	
	/* Make shader object */
	VertShader = glCreateShader(GL_VERTEX_SHADER);
	FragShader = glCreateShader(GL_FRAGMENT_SHADER);
	
	/* reader shader source */
	glShaderSource(VertShader, 1, &text_vertex, 0);
	glShaderSource(FragShader, 1, &text_fragment, 0);
	
	/* Compile and link  shader */
	CompileLinkShader(Gl2Program, &VertShader, &FragShader);
	
	/* Release shader object */
	glDeleteShader(VertShader);
	glDeleteShader(FragShader);
	
	return;
}
