/*
//  kemoviewer_gl.c
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#include <stdio.h>
#include "kemoviewer_gl.h"

/*  OpenGL routines */

void kemoview_allocate_gl_pointers(struct kemoviewer_type *kemoviewer){
    kemoviewer->kemo_shaders = init_kemoview_shaders();
    kemoviewer->kemo_VAOs = init_kemoview_VAOs();
    kemoviewer->menu_VAO = (struct VAO_ids *) malloc(sizeof(struct VAO_ids));
    return;
}

void kemoview_deallocate_gl_pointers(struct kemoviewer_type *kemoviewer){
    clear_kemoview_VAOs(kemoviewer->kemo_VAOs);
    dealloc_kemoview_VAOs(kemoviewer->kemo_VAOs);
    dealloc_kemoview_shaders(kemoviewer->kemo_shaders);
    return;
}



void kemoview_gl_init_lighting(struct kemoviewer_type *kemoviewer){
    kemo_gl_initial_lighting_c(kemoviewer->kemo_shaders);
    assign_kemoview_VAOs(kemoviewer->kemo_VAOs);
    return;
}
void kemoview_gl_background_color(struct kemoviewer_type *kemoviewer){
    set_gl_bg_color(kemoviewer->kemo_mesh->bg_color);
    return;
}

void kemoview_init_gl_background_color(struct kemoviewer_type *kemoviewer){
    init_bg_color_kemoview(kemoviewer->kemo_mesh->bg_color,
                           kemoviewer->kemo_mesh->text_color);
    set_bg_color_kemoview(kemoviewer->kemo_mesh->bg_color,
                          kemoviewer->kemo_mesh->text_color);
    return;
};


void kemoview_mono_view(void){
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();
    modify_mono_kemoview(kemo_sgl);
};
void kemoview_full_modify_view(void){
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();
    modify_stereo_kemoview(FULL_DRAW, kemo_sgl);
};
void kemoview_fast_modify_view(void){
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();
    modify_stereo_kemoview(FAST_DRAW, kemo_sgl);
};

unsigned char * kemoview_alloc_img_buffer_to_bmp(int npix_x, int npix_y){
    unsigned char *image = alloc_img_buffer_to_bmp(npix_x, npix_y);
    return image;
};
void kemoview_get_gl_buffer_to_bmp(int npix_x, int npix_y, unsigned char *image){
    get_gl_buffer_to_bmp(npix_x, npix_y, image);
};
void kemoview_add_quilt_img(unsigned char *glimage, unsigned char *image_quilt){
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();
    get_gl_buffer_to_bmp(kemo_sgl->view_s->nx_frame, kemo_sgl->view_s->ny_frame, glimage);
    set_gl_quilt_bitmap(kemo_sgl->view_s->num_columns, kemo_sgl->view_s->num_raws,
                        kemo_sgl->view_s->istep_quilt,
                        kemo_sgl->view_s->nx_frame, kemo_sgl->view_s->ny_frame,
                        glimage, image_quilt);
    return;
};


/*  Routines using libpng */
#ifdef PNG_OUTPUT
int kemoview_set_image_file_format_id(struct kv_string *image_ext){
    return set_image_format_id_by_ext(image_ext->string);
}

void kemoview_write_window_to_file(int iflag_img, struct kv_string *image_prefix,
                                   int npix_x, int npix_y, unsigned char *image){
    write_gl_window_to_file(iflag_img, image_prefix->string,
                            npix_x, npix_y, image);
}
void kemoview_write_window_to_file_w_step(int iflag_img, int istep, struct kv_string *image_prefix,
                                          int npix_x, int npix_y, unsigned char *image){
    write_gl_window_step_file(iflag_img, istep, image_prefix->string,
                              npix_x, npix_y, image);
}

void kemoview_set_texture_to_PSF(int img_fmt, struct kv_string *image_prefix){
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();
    set_texture_to_psf(img_fmt, image_prefix->string,
                       kemo_sgl->kemo_psf->psf_m[kemo_sgl->kemo_psf->psf_a->id_current]);
};
#endif

/*  Old routines */

struct shader_ids sampleShader;

void kemoview_draw_menu_setup(struct kemoviewer_type *kemoviewer){
    init_gl_menu_setup(kemoviewer->kemo_shaders);
    return;
};

void kemo_Cleanup(struct kemoviewer_type *kemoviewer)
{
  destory_shaders(kemoviewer->kemo_shaders->simple);
  destory_shaders(kemoviewer->kemo_shaders->simple);
}
