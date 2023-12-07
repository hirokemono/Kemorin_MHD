
/* view_modifier_glfw.h */

#ifndef VIEW_MODIFIER_GLFW_
#define VIEW_MODIFIER_GLFW_

#include <math.h>
#include <stdio.h>

#ifndef DEPENDENCY_CHECK
  #include <GLFW/glfw3.h>
#endif

#include "kemoviewer_gl.h"
#include "calypso_GTK.h"

#define ZOOM    1
#define PAN     2
#define ROTATE  3
#define SCALE   4
#define WALKTO  5

struct window_pointers{
	GLFWwindow *glfw_window;
	
	GtkWidget *window_main;
	
	int iflag_menu_update;
};

/* prototypes */

GLFWwindow * open_kemoviwer_glfw_window(int npixel_x, int npixel_y);

void glfw_callbacks_init();
void set_GLFWindowSize(int width, int height);

void draw_fast(struct kemoviewer_type *kemo_sgl);
void draw_full(struct kemoviewer_type *kemo_sgl);
void draw_quilt(struct kemoviewer_type *kemo_sgl);

void sel_write_rotate_views(struct kemoviewer_type *kemo_sgl,
                            int iflag_img, struct kv_string *image_prefix,
                            int i_axis, int inc_deg);
void sel_write_evolution_views(struct kemoviewer_type *kemo_sgl,
                               int iflag_img, struct kv_string *image_prefix,
                               int ist_udt, int ied_udt, int inc_udt);

void set_GLFW_viewtype_mode(int selected);
#endif
