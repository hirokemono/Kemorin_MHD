/*
//  movie_from_GLFW_by_FFMPEG.h
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#include <math.h>
#include <stdio.h>
#include <sys/time.h>

#include <GLFW/glfw3.h>

#include "kemoviewer_gl.h"
#include "render_on_GLFW.h"

#ifdef FFMPEG
  #include "kemoview_FFMPEG_encoder.h"
#endif

#ifndef MOVIE_FROM_GLFW_BY_FFMPEG_
#define MOVIE_FROM_GLFW_BY_FFMPEG_


/*  prototypes */

void sel_lc_write_rotate_views(GLFWwindow *glfw_win,
                               struct kemoviewer_gl_type *kemo_gl,
                               int iflag_img, struct kv_string *image_prefix,
                               int i_fps, int i_axis, int inc_deg);
void sel_lc_write_evolution_views(GLFWwindow *glfw_win,
                                  struct kemoviewer_gl_type *kemo_gl,
                                  int iflag_img, struct kv_string *image_prefix,
                                  int i_fps, int ist_udt, int ied_udt, int inc_udt);

#endif /* MOVIE_FROM_GLFW_BY_FFMPEG_*/
