
/* set_texture_4_psf.h */

#ifndef SET_TEXTURE_4_PSF_
#define SET_TEXTURE_4_PSF_

#include "kemoviewer_param_c.h"
#include "m_kemoview_psf_menu.h"


/* Prototypes */
void flip_gl_bitmap(int num_x, int num_y,
                    unsigned char *glimage, unsigned char *fliped_img);
void flip_gl_bitmap_to_img2d(int num_x, int num_y,
                             unsigned char *glimage, unsigned char **img_2d);
void quilt_bitmap_by_bgra(int n_quilt_column, int n_quilt_raw, int istep_quilt,
                          int npix_each_x, int npix_each_y, unsigned char *bgra,
                          unsigned char *fliped_quilt);
 void set_gl_quilt_bitmap(int n_quilt_column, int n_quilt_raw, int istep_quilt,
                         int npix_each_x, int npix_each_y, unsigned char *glimage, unsigned char *image_quilt);


void set_texture_4_psf(int width, int height, const unsigned char *bgra_in,
			struct psf_menu_val *psf_m);
void release_texture_4_psf(struct psf_menu_val *psf_m);

#endif
