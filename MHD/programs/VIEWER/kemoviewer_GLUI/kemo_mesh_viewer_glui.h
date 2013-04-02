
/* kemo_mesh_viewer_glui.h */

#ifndef KEMO_MESH_VIEWER_GLUI_
#define KEMO_MESH_VIEWER_GLUI_


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include <GL/glui.h>

#include "kemoviewer.h"
#include "view_modifier_glut.h"
#include "const_viewer_menu_glut.h"
#include "kemoview_glut_routines.h"
#include "kemoview_glui_window_input.h"
#include "kemoview_fileselector_glui.h"

/* prototypes */

void draw_mesh_kemo_glui(int iflag_streo_shutter, int iflag_dmesh);

#endif
