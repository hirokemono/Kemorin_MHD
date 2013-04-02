
/* kemoview_glut_routines.h*/

#ifndef KEMOVIEW_GLUT_ROUTINES_
#define KEMOVIEW_GLUT_ROUTINES_

#include <stdio.h>

#include "kemoviewer.h"
#include "view_modifier_glut.h"

/* prototypes */

#ifdef __cplusplus
extern "C" {
#endif

void set_main_window_id_glut(int winid);

void draw_mesh_keep_menu();
void write_rotate_views_glut(int id_image, char *image_head, int i_axis);
void write_evolution_views_glut(int iflag_img, char *image_head, 
								int ist_udt, int ied_udt, int inc_udt);

void set_viewtype_mode_glut(int selected, char *viewtype_title);

#ifdef __cplusplus
}
#endif

#endif
