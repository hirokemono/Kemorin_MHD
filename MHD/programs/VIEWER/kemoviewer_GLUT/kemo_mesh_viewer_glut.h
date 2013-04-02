
/* kemo_mesh_viewer_glut.h */

#ifndef KEMO_MESH_VIEWER_GLUT_
#define KEMO_MESH_VIEWER_GLUT_


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "kemoviewer.h"
#include "view_modifier_glut.h"
#include "const_viewer_menu_glut.h"
#include "kemoview_glut_console_input.h"
#include "kemoview_glut_routines.h"


#ifdef FC_NAME_LOWER_USCORE
#define draw_mesh_kemo     draw_mesh_kemo_
#elif  FC_NAME_UPPER
#define draw_mesh_kemo     DRAW_MESH_KEMO
#elif FC_NAME_UPPER_STDCALL
#define draw_mesh_kemo     DRAW_MESH_KEMO
#else
#define draw_mesh_kemo     draw_mesh_kemo
#endif 

/* prototypes */

void draw_mesh_kemo(int iflag_streo_shutter, int iflag_dmesh);

#endif
