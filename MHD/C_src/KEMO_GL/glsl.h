/*
  glsl.c
*/

#ifndef GLSL__
#define GLSL__

#include "kemoviewer.h"

/* prptotypes */

int glslInit(void);
void LoadShaderFromFile(GLuint *Gl2Program,
			const char *fname_vertex, const char *fname_fragment);
void LoadShaderFromStrings(GLuint *Gl2Program,
			const GLchar *text_vertex, const GLchar *text_fragment);

#endif
