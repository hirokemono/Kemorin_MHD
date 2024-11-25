
/* view_modifier_glfw.h */

#ifndef VIEW_MODIFIER_GLFW_
#define VIEW_MODIFIER_GLFW_

#include <math.h>
#include <stdio.h>
#include <sys/time.h>

#ifndef DEPENDENCY_CHECK
  #include <GLFW/glfw3.h>
#endif

#include "kemoviewer_gl.h"
#include "render_on_GLFW.h"

#ifdef FFMPEG
  #include "movie_from_GLFW_by_FFMPEG.h"
#endif

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

void draw_full_gl(struct kemoviewer_gl_type *kemo_gl);
void draw_fast_gl(struct kemoviewer_gl_type *kemo_gl);

void draw_quilt(int istep_qult, struct kemoviewer_gl_type *kemo_gl);

double draw_rotate_views(struct kemoviewer_gl_type *kemo_gl,
                         int i_axis, int inc_deg, int num_rotation);
void draw_evolution_views(struct kemoviewer_gl_type *kemo_gl,
                          int ist_udt, int ied_udt, int inc_udt);

void sel_write_rotate_views(struct kemoviewer_gl_type *kemo_gl,
                            int iflag_img, struct kv_string *image_prefix,
                            int i_fps, int i_axis, int inc_deg);
void sel_write_evolution_views(struct kemoviewer_gl_type *kemo_gl,
                               int iflag_img, struct kv_string *image_prefix,
                               int i_fps, int ist_udt, int ied_udt, int inc_udt);
#endif
