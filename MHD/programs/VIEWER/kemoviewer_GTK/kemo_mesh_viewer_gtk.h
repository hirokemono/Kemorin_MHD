
/* kemo_mesh_viewer_gtk.h */

#ifndef KEMO_MESH_VIEWER_GLUT_
#define KEMO_MESH_VIEWER_GLUT_


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include <gtk/gtk.h>

#include "kemoviewer.h"
#include "kemoview_gtk_PSF_menu.h"
#include "kemoview_gtk_mesh_menu.h"
#include "kemoview_gtk_preference_menu.h"
#include "kemoview_fileselector_gtk.h"
#include "kemoview_gtk_main_menu.h"
#include "kemoview_gtk_viewmatrix_menu.h"

/* prototypes */

int draw_mesh_kemo(int iflag_streo_shutter, int iflag_dmesh);

#endif
