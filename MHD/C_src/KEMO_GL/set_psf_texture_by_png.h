
/* set_psf_texture_by_png.h */

#ifndef SET_PSF_TEXTURE_BY_PNG_
#define SET_PSF_TEXTURE_BY_PNG_

#include "m_kemoview_psf_menu.h"
#include "read_image_2_png.h"
#include "read_image_2_bmp.h"
#include "skip_comment_c.h"


/* Prototypes */
void set_texture_to_psf(int img_fmt, const char *img_head, struct psf_menu_val *psf_m);

#endif
