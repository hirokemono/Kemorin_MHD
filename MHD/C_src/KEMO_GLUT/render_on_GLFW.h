/*
//  render_on_GLFW.h
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#include <math.h>
#include <stdio.h>
#include <sys/time.h>

#include <GLFW/glfw3.h>

#include "kemoviewer_gl.h"

#ifndef RENDER_ON_GLFW_
#define RENDER_ON_GLFW_

/*  prototypes */

void draw_full_lc(GLFWwindow *glfw_win,
                  struct kemoviewer_type *kemo_sgl,
                  struct kemoviewer_gl_type * kemo_gl);
void draw_quilt_lc(int istep_qult, GLFWwindow *glfw_win,
                   struct kemoviewer_type *kemo_sgl,
                   struct kemoviewer_gl_type * kemo_gl);
void draw_fast_lc(GLFWwindow *glfw_win,
                  struct kemoviewer_type *kemo_sgl,
                  struct kemoviewer_gl_type * kemo_gl);

double draw_rotate_views_lc(GLFWwindow *glfw_win,
                            struct kemoviewer_type *kemo_sgl,
                            struct kemoviewer_gl_type *kemo_gl,
                            int i_axis, int inc_deg, int num_rotation);
void draw_evolution_views_lc(GLFWwindow *glfw_win,
                             struct kemoviewer_type *kemo_sgl,
                             struct kemoviewer_gl_type *kemo_gl,
                             int ist_udt, int ied_udt, int inc_udt);

void write_rotate_views(GLFWwindow *glfw_win,
                        struct kemoviewer_type *kemo_sgl,
                        struct kemoviewer_gl_type *kemo_gl,
                        int ied_deg, int inc_deg,
                        int iflag_img, struct kv_string *image_prefix,
                        int npix_x, int npix_y, unsigned char *image);
void write_rotate_quilt_views(GLFWwindow *glfw_win,
                              struct kemoviewer_type *kemo_sgl,
                              struct kemoviewer_gl_type *kemo_gl,
                              int ied_deg, int inc_deg,
                              int iflag_img, struct kv_string *image_prefix,
                              int npix_x, int npix_y, unsigned char *image);

void write_evolution_quilt_views(GLFWwindow *glfw_win,
                                 struct kemoviewer_type *kemo_sgl,
                                 struct kemoviewer_gl_type *kemo_gl,
                                 int ist_udt, int ied_udt, int inc_udt,
                                 int iflag_img, struct kv_string *image_prefix,
                                 int npix_x, int npix_y, unsigned char *image);

void write_evolution_views(GLFWwindow *glfw_win,
                           struct kemoviewer_type *kemo_sgl,
                           struct kemoviewer_gl_type *kemo_gl,
                           int ist_udt, int ied_udt, int inc_udt,
                           int iflag_img, struct kv_string *image_prefix,
                           int npix_x, int npix_y, unsigned char *image);

#endif /* RENDER_ON_GLFW_ */
