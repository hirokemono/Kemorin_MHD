
/* kemo_mesh_viewer_gtk.h */

#ifndef KEMO_MESH_VIEWER_GLUT_
#define KEMO_MESH_VIEWER_GLUT_


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "gtk/gtk.h"

#include "kemoviewer.h"
#include "view_modifier_glut.h"
#include "const_viewer_menu_glut.h"
#include "kemoview_glut_routines.h"
#include "kemoview_gtk_window_input.h"
#include "kemoview_fileselector_gtk.h"

/* prototypes */

void draw_mesh_kemo(int iflag_streo_shutter, int iflag_dmesh);

#endif
