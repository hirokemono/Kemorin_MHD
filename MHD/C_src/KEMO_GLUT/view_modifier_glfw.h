
/* view_modifier_glfw.h */

#ifndef VIEW_MODIFIER_GLFW_
#define VIEW_MODIFIER_GLFW_

#include <math.h>
#include <stdio.h>

#ifndef DEPENDENCY_CHECK
  #include <GLFW/glfw3.h>
#endif

#include "calypso_GTK.h"
#include "kemoviewer.h"

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

int draw_fast(void);
void draw_full(void);
void write_rotate_views(int iflag_img, struct kv_string *image_prefix, 
                             int i_axis, int inc_deg);
void write_evolution_views(int iflag_img, struct kv_string *image_prefix, 
								int ist_udt, int ied_udt, int inc_udt);

void set_viewtype_mode(int selected);
#endif
