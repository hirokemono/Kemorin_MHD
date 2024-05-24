//
//  kemoviewer_gl.h
//  
//
//  Created by Hiroaki Matsui on 11/26/23.
//

#ifndef kemoviewer_gl_h_
#define kemoviewer_gl_h_

#ifdef __APPLE__
#include<OpenGL/gl3.h>
#else
#include<GL/gl.h>
#endif

#include "kemoviewer.h"

#include "glsl.h"
#include "vartex_array_object_gl.h"

#include "m_kemoviewer_data.h"
#include "m_transfer_matrices.h"
#include "m_gl_transfer_matrix.h"
#include "init_gl_lighting_c.h"
#include "kemoviewer_param_c.h"
#include "move_draw_objects_gl.h"
#include "write_gl_window_to_file.h"
#include "set_kemoviewer_ucd_data.h"

#ifdef PNG_OUTPUT
    #include "set_psf_texture_by_png.h"
#endif


#ifdef __cplusplus
extern "C" {
#endif


/*  OopenGL routines */
    struct kemoviewer_gl_type * kemoview_allocate_gl_pointers(void);
    void kemoview_deallocate_gl_pointers(struct kemoviewer_gl_type *kemo_gl);

    void kemoview_gl_init_lighting(struct kemoviewer_gl_type *kemo_gl);

    void kemoview_gl_background_color(struct kemoviewer_type *kemoviewer);
    void kemoview_init_gl_background_color(struct kemoviewer_type *kemoviewer);

    void kemoview_modify_view(struct kemoviewer_type *kemo_sgl,
                              struct kemoviewer_gl_type *kemo_gl);
    void kemoview_modify_anaglyph(struct kemoviewer_type *kemo_sgl,
                                  struct kemoviewer_gl_type *kemo_gl);

    unsigned char * kemoview_alloc_RGB_buffer_to_bmp(int npix_x, int npix_y);
    void kemoview_get_gl_buffer_to_bmp(int npix_x, int npix_y, unsigned char *image);
    void kemoview_add_quilt_img(int istep_quilt, struct kemoviewer_type *kemo_sgl,
                                unsigned char *glimage, unsigned char *image_quilt);


/* subroutines for IO */
    int kemoview_set_data_format_flag(struct kv_string *filename,
                                      struct kv_string *stripped_prefix,
                                      struct kv_string *stripped_ext);
    void kemoview_get_ext_from_file_name(struct kv_string *filename,
                                         struct kv_string *stripped_prefix,
                                         struct kv_string *stripped_ext);
    void kemoview_add_ext_to_file_name(struct kv_string *file_prefix,
                                       struct kv_string *added_ext,
                                       struct kv_string *file_name);

/* subroutines for surafces */
    int kemoview_get_PSF_file_prefix(struct kemoviewer_type *kemoviewer,
                                     struct kv_string *stripped_filehead);
    void kemoview_release_PSF_gl_texture(struct kemoviewer_type *kemo_sgl,
                                         struct kemoviewer_gl_type *kemo_gl);
    void * kemoview_link_active_colormap_param(int i_current, int icomp,
                                               struct kemoviewer_type *kemoviewer);

#ifdef PNG_OUTPUT
/* Load texture onto current sectioning image */
    void kemoview_set_texture_to_PSF(int img_fmt, struct kv_string *image_prefix,
                                     struct kemoviewer_type *kemo_sgl,
                                     struct kemoviewer_gl_type *kemo_gl);

/* Set Image file format by ID */
    int kemoview_set_image_file_format_id(struct kv_string *image_ext);
/* Write Kemoviwer window image to file without step number */
    void kemoview_write_window_to_file(int iflag_img, struct kv_string *image_prefix,
                                       int npix_x, int npix_y, unsigned char *image);
/* Write Kemoviwer window image to file with step number */
    void kemoview_write_window_to_file_w_step(int iflag_img, int istep, struct kv_string *image_prefix,
                                              int npix_x, int npix_y, unsigned char *image);

/*  Old routines */
    void kemoview_draw_menu_setup(struct kemoviewer_gl_type *kemo_gl);
    void kemo_Cleanup(struct kemoviewer_gl_type *kemo_gl);
#endif

#ifdef __cplusplus
}
#endif

#endif /* kemoviewer_gl_h_ */
