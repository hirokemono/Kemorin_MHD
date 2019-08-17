
/* ysglusefontbitmap.h */

#ifndef YSGLUSEFONTBITMAP_IS_INCLUDED
#define YSGLUSEFONTBITMAP_IS_INCLUDED
/* { */

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include <string.h>

#ifdef WIN32
#include <windows.h>
#endif

#include "kemoviewer_param_c.h"
#include "ysglfontdata.h"

/* prototypes */

void ysGlPlotBitmap2d(int listBase, GLdouble xbase, GLdouble ybase, GLubyte *text);
void ysGlPlotBitmap2i(int listBase, GLint ixbase, GLint iybase, GLubyte *text);

void YsGlUseFontBitmap6x7(int listBase);
void YsGlUseFontBitmap6x8(int listBase);
void YsGlUseFontBitmap8x8(int listBase);
void YsGlUseFontBitmap8x12(int listBase);
void YsGlUseFontBitmap12x16(int listBase);
void YsGlUseFontBitmap16x20(int listBase);
void YsGlUseFontBitmap16x24(int listBase);
void YsGlUseFontBitmap20x28(int listBase);
void YsGlUseFontBitmap20x32(int listBase);

#ifdef __cplusplus
#include "ysglfontdata.h"
}
#endif


/* } */
#endif
