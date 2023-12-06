/*
//  kemoviewer_gl.c
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#include <stdio.h>
#include "kemoviewer_gl.h"

struct kemoviewer_gl_type *kemo_sgl_gl;

/*  OpenGL routines */
struct kemoviewer_gl_type * kemoview_single_gl_type(void){return kemo_sgl_gl;};

struct kemoviewer_gl_type * kemoview_allocate_gl_pointers(void){
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) malloc(sizeof(struct kemoviewer_gl_type));
    if(kemo_gl == NULL){
        printf("malloc error for kemoviewer_gl_type\n");
        exit(0);
    };
    kemo_gl->kemo_shaders = init_kemoview_shaders();
    kemo_gl->kemo_VAOs = init_kemoview_VAOs();

    if((kemo_gl->menu_VAO = (struct VAO_ids *) malloc(sizeof(struct VAO_ids))) == NULL){
        printf("malloc error for menu_VAO\n");
        exit(0);
    };
    
    kemo_sgl_gl = kemo_gl;
    return kemo_sgl_gl;
}

void kemoview_deallocate_gl_pointers(struct kemoviewer_gl_type *kemo_gl){
    free(kemo_gl->menu_VAO);
    clear_kemoview_VAOs(kemo_gl->kemo_VAOs);
    dealloc_kemoview_VAOs(kemo_gl->kemo_VAOs);
    dealloc_kemoview_shaders(kemo_gl->kemo_shaders);
    return;
}

void kemoview_gl_init_lighting(struct kemoviewer_gl_type *kemo_gl){
    kemo_gl_initial_lighting_c(kemo_gl->kemo_shaders);
    assign_kemoview_VAOs(kemo_gl->kemo_VAOs);
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


void kemoview_modify_view(void){
    struct kemoviewer_type *kemo_sgl =   kemoview_single_viwewer_struct();
    struct kemoviewer_gl_type *kemo_gl = kemoview_single_gl_type();

    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
    update_draw_objects_gl3(kemo_sgl, kemo_gl);
};
void kemoview_modify_anaglyph(struct line_text_image *left_image,
                              struct line_text_image *right_image){
    struct kemoviewer_type *kemo_sgl =   kemoview_single_viwewer_struct();
    struct kemoviewer_gl_type *kemo_gl = kemoview_single_gl_type();
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
    kemo_sgl->kemo_buffers->message_image->text_opacity = 0.8;
    kemo_sgl->kemo_buffers->screen_buf->num_nod_buf = 6;
    draw_anaglyph_2D_VAO(kemo_sgl, kemo_gl,
                         left_image, right_image);
    kemo_sgl->kemo_buffers->screen_buf->num_nod_buf = 0;
};


unsigned char * kemoview_alloc_RGB_buffer_to_bmp(int npix_x, int npix_y){
    unsigned char *image = alloc_RGB_buffer_to_bmp(npix_x, npix_y);
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


void kemoview_release_PSF_gl_texture(void){
    struct kemoviewer_type *kemo_sgl = kemoview_single_viwewer_struct();
    struct kemoviewer_gl_type *kemo_gl = kemoview_single_gl_type();
    release_PSF_texture_from_gl(kemo_sgl->kemo_psf->psf_a->psf_texure,
                                kemo_gl->kemo_shaders->texture_name);
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
    struct kemoviewer_gl_type *kemo_gl = kemoview_single_gl_type();
    set_texture_to_psf(img_fmt, image_prefix->string,
                       kemo_sgl->kemo_psf->psf_a->psf_texure,
                       &kemo_gl->kemo_shaders->texture_name);
};
#endif

/*  Old routines */

struct shader_ids sampleShader;

void kemoview_draw_menu_setup(struct kemoviewer_gl_type *kemo_gl){
    init_gl_menu_setup(kemo_gl->kemo_shaders);
    return;
};

void kemo_Cleanup(struct kemoviewer_gl_type *kemo_gl)
{
  destory_shaders(kemo_gl->kemo_shaders->simple);
}
