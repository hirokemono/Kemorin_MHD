
/* gl_buffer_2_gl2ps.h */

#ifndef GL_BUFFER_2_GL2PS_
#define GL_BUFFER_2_GL2PS_

#include "gl2ps.h"
#include "kemoviewer.h"

/* Prototype */

void gl_buffer_2_ps_gl2ps(int size, int doSort, const char *filehead);
void gl_buffer_2_eps_gl2ps(int size, int doSort, const char *filehead);
void gl_buffer_2_pdf_gl2ps(int size, int doSort, const char *filehead);

#endif
