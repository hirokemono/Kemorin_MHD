
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
void write_rotate_views_glut(int iflag_img, struct kv_string *image_prefix, 
                             int i_axis, int inc_deg);
void write_evolution_views_glut(int iflag_img, struct kv_string *image_prefix, 
								int ist_udt, int ied_udt, int inc_udt);

void set_viewtype_mode_glut(int selected);

#ifdef __cplusplus
}
#endif

#endif
