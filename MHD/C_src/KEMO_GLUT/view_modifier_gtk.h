
/* view_modifier_gtk.h */

#ifndef VIEW_MODIFIER_GTK_
#define VIEW_MODIFIER_GTK_

#include <math.h>
#include <stdio.h>
#include <gtk/gtk.h>

#include "kemoviewer.h"

#define ZOOM    1
#define PAN     2
#define ROTATE  3
#define SCALE   4
#define WALKTO  5

/* prototypes */

GLFWwindow * open_kemoviwer_window(int npixel_x, int npixel_y);

void gtk_view_modifier_init(GLFWwindow* window);
void set_GtkWindowSize(int width, int height);

void draw_fast_gtk(void);
void draw_mesh_gtk(void);
void write_rotate_views_gtk(int iflag_img, struct kv_string *image_prefix, 
                             int i_axis, int inc_deg);
void write_evolution_views_gtk(int iflag_img, struct kv_string *image_prefix, 
								int ist_udt, int ied_udt, int inc_udt);

void set_viewtype_mode_glfw(int selected);

#endif
