
/* kemo_mesh_viewer_glut.h */

#ifndef KEMO_MESH_VIEWER_GLUT_
#define KEMO_MESH_VIEWER_GLUT_


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "kemoviewer.h"
#include "m_kemoview_psf_menu.h"
#include "view_modifier_glut.h"
#include "const_viewer_menu_glut.h"
#include "kemoview_glut_console_input.h"


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

void set_window_id_glut3(int *id3_window, int *id3_menu);
void link_glut_menu_address();
void draw_mesh_w_menu();

void draw_mesh_kemo(int iflag_streo_shutter, int iflag_dmesh);

#endif
