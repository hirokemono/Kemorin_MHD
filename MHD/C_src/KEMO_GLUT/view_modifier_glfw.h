
/* view_modifier_glfw.h */

#ifndef VIEW_MODIFIER_GLFW_
#define VIEW_MODIFIER_GLFW_

#include <math.h>
#include <stdio.h>
#include <GLFW/glfw3.h>
#include <gtk/gtk.h>

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

#ifdef __cplusplus
extern "C" {
#endif

GLFWwindow * open_kemoviwer_window(int npixel_x, int npixel_y);

void glfw_view_modifier_init(GLFWwindow* window);
void display(GLFWwindow* window);
void modifywindow(int width, int height);

void draw_fast_glfw(void);
void draw_mesh_glfw(void);
void write_rotate_views_glut(int iflag_img, struct kv_string *image_prefix, 
                             int i_axis, int inc_deg);
void write_evolution_views_glut(int iflag_img, struct kv_string *image_prefix, 
								int ist_udt, int ied_udt, int inc_udt);

void set_viewtype_mode_glfw(int selected);

#ifdef __cplusplus
}
#endif

#endif
