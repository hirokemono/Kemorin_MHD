
/* set_texture_4_psf.h */

#ifndef SET_TEXTURE_4_PSF_
#define SET_TEXTURE_4_PSF_

#include "m_kemoviewer_menu.h"
#include "read_image_2_png.h"
#include "read_image_2_bmp.h"
#include "skip_comment_c.h"


/* Prototypes */
int set_texture_4_psf(int img_fmt, const char *img_head, struct psf_menu_val *psf_m);
void release_texture_4_psf(struct psf_menu_val *psf_m);

#endif
