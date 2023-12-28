
/* set_psf_texture_by_png.c */

#include "set_psf_texture_by_png.h"

static int set_texture_from_png(const char *img_head, struct gl_texure_image *psf_texure) {
	int iflag_rgba;
    int nipxel_xy[2];
	read_png_file_c(img_head, &nipxel_xy[0], &nipxel_xy[1], &iflag_rgba);
	alloc_draw_psf_texture(nipxel_xy[0], nipxel_xy[1], psf_texure);
	copy_rgba_from_png_c(psf_texure->nipxel_xy[0], psf_texure->nipxel_xy[1],
                         iflag_rgba, psf_texure->texure_rgba);
	return 1;
}

static int set_texture_from_bmp(const char *img_head, struct gl_texure_image *psf_texure) {
    struct BMP_data *d_BMP = read_BMP_c(img_head);
	alloc_draw_psf_texture(d_BMP->ihpixf, d_BMP->jvpixf, psf_texure);
	copy_rgba_from_BMP_c(d_BMP, psf_texure->texure_rgba);
    dealloc_BMP_data(d_BMP);
    return 1;
}

void set_texture_to_psf(int img_fmt, const char *img_head,
                        struct gl_texure_image *psf_texure,
                        unsigned int *texture_name){
    int iflag = 0;
	if(img_fmt == SAVE_PNG){
        iflag = set_texture_from_png(img_head, psf_texure);
	}else if (img_fmt == SAVE_BMP){
        iflag = set_texture_from_bmp(img_head, psf_texure);
	};
    if(iflag > 0){glGenTextures(1 , texture_name);};
	return;
}
