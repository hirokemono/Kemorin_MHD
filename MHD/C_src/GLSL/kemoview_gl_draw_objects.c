/*
 *  kemoview_gl_draw_objects.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gl_draw_objects.h"

void update_draw_objects_gl3(struct kemoviewer_type *kemoview,
                             struct kemoview_VAOs *kemo_VAOs,
                             struct kemoview_shaders *kemo_shaders){
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
    update_draw_objects_gl(kemoview->kemo_psf, kemoview->kemo_fline,
                           kemoview->kemo_mesh, kemoview->view_s,
                           kemoview->kemo_buffers, kemo_VAOs, kemo_shaders);
	return;
}

void draw_objects_to_rgb_gl(struct kemoviewer_type *kemoview,
                            struct kemoview_VAOs *kemo_VAOs,
                            struct kemoview_shaders *kemo_shaders,
                            struct gl_texure_image *image){
    alloc_draw_psf_texture(kemoview->view_s->nx_frame,
                           kemoview->view_s->ny_frame,
                           image);

    glDrawBuffer(GL_BACK);
    update_draw_objects_gl3(kemoview, kemo_VAOs, kemo_shaders);
    glReadBuffer(GL_BACK);
    glPixelStorei(GL_PACK_ALIGNMENT, IONE);
    glReadPixels(IZERO, IZERO, image->nipxel_xy[0], image->nipxel_xy[1],
                 GL_RGB, GL_UNSIGNED_BYTE, image->texure_rgba);
    return;
};

void draw_anaglyph_to_rgb_gl(struct kemoviewer_type *kemoview,
                             struct kemoview_VAOs *kemo_VAOs,
                             struct kemoview_shaders *kemo_shaders,
                             struct gl_texure_image *anaglyph_image){
    struct gl_texure_image *left_img =  alloc_kemoview_gl_texure();
    struct gl_texure_image *right_img = alloc_kemoview_gl_texure();

    alloc_draw_psf_texture(kemoview->view_s->nx_frame,
                           kemoview->view_s->ny_frame,
                           anaglyph_image);

    modify_left_viewmat(kemoview->view_s);
    draw_objects_to_rgb_gl(kemoview, kemo_VAOs, kemo_shaders,
                           left_img);
    
    modify_right_viewmat(kemoview->view_s);
    draw_objects_to_rgb_gl(kemoview, kemo_VAOs, kemo_shaders,
                           right_img);

    half_anaglyph_rgba_by_rgbs(left_img->nipxel_xy[0], left_img->nipxel_xy[1],
                               left_img->texure_rgba, right_img->texure_rgba,
                               anaglyph_image->texure_rgba);
    dealloc_kemoview_gl_texure(left_img);
    dealloc_kemoview_gl_texure(right_img);
    return;
};


static void move_draw_anaglyph_gl3(struct kemoviewer_type *kemoview,
                                   struct kemoview_VAOs *kemo_VAOs,
                                   struct kemoview_shaders *kemo_shaders,
                                   struct gl_texure_image *anaglyph_image){
    const_screen_buffer(kemoview->view_s->iflag_view_type,
                        kemoview->view_s->nx_frame,
                        kemoview->view_s->ny_frame,
                        kemoview->kemo_buffers->screen_buf);

    Const_texture_VAO(anaglyph_image, kemoview->kemo_buffers->screen_buf,
                      kemo_VAOs->screen_VAO);

    double *orthogonal = orthogonal_projection_mat_c(0.0, kemoview->view_s->nx_frame,
                                                     0.0, kemoview->view_s->ny_frame,
                                                     -1.0, 1.0);
    struct transfer_matrices *cbar_matrices = plane_transfer_matrices(orthogonal);
    free(orthogonal);

    /* draw message */
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
    draw_textured_2D_box_VAO(cbar_matrices, kemo_VAOs->screen_VAO, kemo_shaders);
    free(cbar_matrices);
    return;
}

void select_modify_anaglyph(struct kemoviewer_type *kemoview,
                            struct kemoview_VAOs *kemo_VAOs,
                            struct kemoview_shaders *kemo_shaders){
    if(kemoview->view_s->iflag_view_type == VIEW_STEREO){
        struct gl_texure_image *anaglyph_image = alloc_kemoview_gl_texure();
        draw_anaglyph_to_rgb_gl(kemoview, kemo_VAOs, kemo_shaders,
                                anaglyph_image);
        
        glDrawBuffer(GL_BACK);
        move_draw_anaglyph_gl3(kemoview, kemo_VAOs, kemo_shaders,
                               anaglyph_image);
        dealloc_kemoview_gl_texure(anaglyph_image);
    }else{
        modify_mono_viewmat(kemoview->view_s);
        glDrawBuffer(GL_BACK);
        update_draw_objects_gl3(kemoview, kemo_VAOs, kemo_shaders);
    }
};
