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

#include "m_kemoviewer_data.h"
#include "init_gl_lighting_c.h"
#include "move_draw_objects_gl.h"
#include "modify_stereo_view_gl.h"
#include "write_gl_window_to_file.h"

#ifdef PNG_OUTPUT
    #include "set_psf_texture_by_png.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif


/*  OopenGL routines */

    void kemoview_allocate_gl_pointers(struct kemoviewer_type *kemoviewer);
    void kemoview_deallocate_gl_pointers(struct kemoviewer_type *kemoviewer);

    void kemoview_gl_init_lighting(struct kemoviewer_type *kemoviewer);
    void kemoview_gl_background_color(struct kemoviewer_type *kemoviewer);
    void kemoview_init_gl_background_color(struct kemoviewer_type *kemoviewer);

    void kemoview_mono_view(void);
    void kemoview_full_modify_view(void);
    void kemoview_fast_modify_view(void);

    unsigned char * kemoview_alloc_img_buffer_to_bmp(int npix_x, int npix_y);
    void kemoview_get_gl_buffer_to_bmp(int npix_x, int npix_y, unsigned char *image);
    void kemoview_add_quilt_img(unsigned char *glimage, unsigned char *image_quilt);

#ifdef PNG_OUTPUT
/* Load texture onto current sectioning image */
    void kemoview_set_texture_to_PSF(int img_fmt, struct kv_string *image_prefix);

/* Set Image file format by ID */
    int kemoview_set_image_file_format_id(struct kv_string *image_ext);
/* Write Kemoviwer window image to file without step number */
    void kemoview_write_window_to_file(int iflag_img, struct kv_string *image_prefix,
                                       int npix_x, int npix_y, unsigned char *image);
/* Write Kemoviwer window image to file with step number */
    void kemoview_write_window_to_file_w_step(int iflag_img, int istep, struct kv_string *image_prefix,
                                              int npix_x, int npix_y, unsigned char *image);

/*  Old routines */
    void kemoview_draw_menu_setup(struct kemoviewer_type *kemoviewer);
    void kemo_Cleanup(struct kemoviewer_type *kemoviewer);
#endif

#ifdef __cplusplus
}
#endif

#endif /* kemoviewer_gl_h_ */
