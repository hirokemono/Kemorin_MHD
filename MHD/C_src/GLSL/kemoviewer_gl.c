/*
//  kemoviewer_gl.c
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
*/

#include <stdio.h>
#include "kemoviewer_gl.h"

/*  OpenGL routines */
struct kemoviewer_gl_type * kemoview_allocate_gl_pointers(struct kemoviewer_type *kemoviewer){
    struct kemoviewer_gl_type *kemo_gl = (struct kemoviewer_gl_type *) malloc(sizeof(struct kemoviewer_gl_type));
    if(kemo_gl == NULL){
        printf("malloc error for kemoviewer_gl_type\n");
        exit(0);
    };
    kemo_gl->kemoview_data = kemoviewer;
    
    kemo_gl->kemo_shaders = init_kemoview_shaders();
    initialize_gl_shaders(kemo_gl->kemo_shaders);

    kemo_gl->kemo_VAOs = init_kemoview_VAOs();
    assign_kemoview_VAOs(kemo_gl->kemo_VAOs);

    return kemo_gl;
}

void kemoview_deallocate_gl_pointers(struct kemoviewer_gl_type *kemo_gl){
    free(kemo_gl->menu_VAO);
    clear_kemoview_VAOs(kemo_gl->kemo_VAOs);
    dealloc_kemoview_VAOs(kemo_gl->kemo_VAOs);
    dealloc_kemoview_shaders(kemo_gl->kemo_shaders);
//    kemo_gl->kemoview_data = NULL;
    free(kemo_gl);
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


void kemoview_gl_full_draw(struct kemoviewer_gl_type * kemo_gl){
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO,
                              kemo_gl->kemoview_data);
    kemoview_set_view_integer(ISET_DRAW_MODE, FULL_DRAW,
                              kemo_gl->kemoview_data);
    select_modify_anaglyph(kemo_gl->kemoview_data,
                           kemo_gl->kemo_VAOs,
                           kemo_gl->kemo_shaders);
    return;
};

void kemoview_gl_fast_draw(struct kemoviewer_gl_type * kemo_gl){
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO,
                              kemo_gl->kemoview_data);
    kemoview_set_view_integer(ISET_DRAW_MODE, SIMPLE_DRAW,
                              kemo_gl->kemoview_data);
    kemoview_mono_viewmatrix(kemo_gl->kemoview_data);
    kemoview_fast_buffers(kemo_gl->kemoview_data);
    glDrawBuffer(GL_BACK);
    update_draw_objects_gl3(kemo_gl->kemoview_data,
                            kemo_gl->kemo_VAOs,
                            kemo_gl->kemo_shaders);
    return;
};

void kemoview_gl_quilt_draw(int istep_qult, struct kemoviewer_gl_type * kemo_gl){
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO,
                              kemo_gl->kemoview_data);
    kemoview_set_view_integer(ISET_DRAW_MODE, QUILT_DRAW,
                              kemo_gl->kemoview_data);
    kemoview_step_viewmatrix(istep_qult, kemo_gl->kemoview_data);
    glDrawBuffer(GL_BACK);
    update_draw_objects_gl3(kemo_gl->kemoview_data,
                            kemo_gl->kemo_VAOs,
                            kemo_gl->kemo_shaders);
    return;
};

void kemoview_modify_anaglyph(struct kemoviewer_gl_type *kemo_gl){
    select_modify_anaglyph(kemo_gl->kemoview_data,
                           kemo_gl->kemo_VAOs,
                           kemo_gl->kemo_shaders);
};



unsigned char * kemoview_alloc_RGB_buffer_to_bmp(int npix_x, int npix_y){
    unsigned char *image = alloc_RGB_buffer_to_bmp(npix_x, npix_y);
    return image;
};
void kemoview_get_gl_buffer_to_bmp(int npix_x, int npix_y, unsigned char *image){
    get_gl_buffer_to_bmp(npix_x, npix_y, image);
    printf("Tako\n");
};
void kemoview_get_gl_buffer_to_bmp2(struct kemoviewer_type *kemo_sgl,
                                    struct kemoview_VAOs *kemo_VAOs,
                                    struct kemoview_shaders *kemo_shaders,
                                    struct gl_texure_image *image_t){
    draw_objects_to_rgb_gl(kemo_sgl, kemo_VAOs, kemo_shaders, image_t);
    printf("Tako2\n");
};
void kemoview_add_quilt_img(int istep_quilt, struct kemoviewer_type *kemo_sgl,
                            unsigned char *glimage, unsigned char *image_quilt){
    get_gl_buffer_to_bmp(kemo_sgl->view_s->nx_frame, kemo_sgl->view_s->ny_frame, glimage);
    set_gl_quilt_bitmap(kemo_sgl->view_s->num_columns, kemo_sgl->view_s->num_raws,
                        istep_quilt, kemo_sgl->view_s->nx_frame, kemo_sgl->view_s->ny_frame,
                        glimage, image_quilt);
    printf("Baka\n");
    return;
};

int kemoview_set_data_format_flag(struct kv_string *filename,
                                  struct kv_string *stripped_prefix,
                                  struct kv_string *stripped_ext){
    alloc_kvstringitem(strlen(filename->string), stripped_prefix);
    alloc_kvstringitem(strlen(filename->string), stripped_ext);
    return set_data_format_flag(filename->string, stripped_prefix->string,
                                stripped_ext->string);
}

void kemoview_get_ext_from_file_name(struct kv_string *filename,
                                     struct kv_string *stripped_prefix,
                                     struct kv_string *stripped_ext){
    alloc_kvstringitem((int) strlen(filename->string), stripped_prefix);
    alloc_kvstringitem((int) strlen(filename->string), stripped_ext);
    get_ext_from_file_name_c(filename->string, stripped_prefix->string,
                             stripped_ext->string);
}
void kemoview_add_ext_to_file_name(struct kv_string *file_prefix,
                                   struct kv_string *added_ext,
                                   struct kv_string *file_name){
    int lentgh = (int) strlen(file_prefix->string) + (int) strlen(added_ext->string);
    alloc_kvstringitem(lentgh+2, file_name);
    add_ext_to_file_name_c(file_prefix->string, added_ext->string, file_name->string);
}



int kemoview_get_PSF_file_prefix(struct kemoviewer_gl_type *kemo_gl,
                                 struct kv_string *stripped_filehead){
    struct kv_string* stripped_dir = alloc_kvstring();
    int istep = send_psf_file_dir_prefix(kemo_gl->kemoview_data->kemo_mul_psf,
                                         stripped_dir, stripped_filehead);
    dealloc_kvstring(stripped_dir);
    return istep;
}

void kemoview_release_PSF_gl_texture(struct kemoviewer_type *kemo_sgl,
                                     struct kemoviewer_gl_type *kemo_gl){
    int iflag = kemo_gl->kemoview_data->kemo_mul_psf->psf_a->ipsf_texured;
    release_PSF_texture_from_gl(iflag, kemo_gl->kemo_shaders);
    return;
};

void * kemoview_link_active_colormap_param(int id_model,
                                           struct kemoviewer_gl_type *kemo_gl){
    struct colormap_params * cmap_params;
    if(id_model == FIELDLINE_RENDERING){
        cmap_params = link_active_colormap_param(kemo_gl->kemoview_data->kemo_fline->fline_m);
    }else if(id_model == TRACER_RENDERING){
        cmap_params =  link_active_colormap_param(kemo_gl->kemoview_data->kemo_tracer->tracer_m);
    }else{
        int i_current = get_curent_PSF_ID(kemo_gl->kemoview_data->kemo_mul_psf->psf_a);
        cmap_params = link_active_colormap_param(kemo_gl->kemoview_data->kemo_mul_psf->psf_m[i_current]);
    }
    return (void *) cmap_params;
}


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
void kemoview_set_texture_to_PSF(int img_fmt, struct kv_string *image_prefix,
                                 struct kemoviewer_type *kemo_sgl,
                                 struct kemoviewer_gl_type *kemo_gl){
    int iflag = set_texture_to_psf(img_fmt, image_prefix->string,
                                   kemo_gl->kemoview_data->kemo_mul_psf->psf_a->psf_texure);
    if(iflag > 0){glGenTextures(1 , &kemo_gl->kemo_shaders->texture_name);};
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
