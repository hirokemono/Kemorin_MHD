
/* view_modifier_glfw.h */

#ifndef VIEW_MODIFIER_GLFW_
#define VIEW_MODIFIER_GLFW_

#include <math.h>
#include <stdio.h>

#ifndef DEPENDENCY_CHECK
  #include <GLFW/glfw3.h>
#endif

#include "kemoviewer_gl.h"
#include "test_shaders.h"

#define ZOOM    1
#define PAN     2
#define ROTATE  3
#define SCALE   4
#define WALKTO  5

/* prototypes */

void set_GLFW_viewtype_mode(int selected);

GLFWwindow * open_kemoviwer_glfw_window(int npixel_x, int npixel_y);

void glfw_callbacks_init(struct kemoviewer_type *kemo_sgl,
                         struct kemoviewer_gl_type *kemo_gl);
void set_GLFWindowSize(int width, int height,
                       struct kemoviewer_type *kemo_sgl);

void draw_fast(struct kemoviewer_type *kemo_sgl);
void draw_full(struct kemoviewer_type *kemo_sgl);
void kemoview_gl_test_init(struct kemoviewer_gl_type *kemo_gl);

#endif
