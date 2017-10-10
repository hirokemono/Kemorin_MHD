
/* set_psf_texture_by_png.c */

#include "set_psf_texture_by_png.h"

static void set_texture_from_png(const char *img_head, struct psf_menu_val *psf_m) {
	int  iflag_rgba;
	
	read_png_file_c(img_head, &psf_m->texture_width, &psf_m->texture_height, &iflag_rgba);
	alloc_draw_psf_texture(psf_m);
	copy_rgba_from_png_c(&psf_m->texture_width, &psf_m->texture_height, 
			&iflag_rgba, psf_m->texture_rgba);
	glGenTextures(1 , &psf_m->texture_name[0]);
	return;
}

static void set_texture_from_bmp(const char *img_head, struct psf_menu_val *psf_m) {
	
	read_BMP_c(img_head, &psf_m->texture_width, &psf_m->texture_height);
	alloc_draw_psf_texture(psf_m);
	copy_rgba_from_BMP_c(psf_m->texture_width, psf_m->texture_height, 
				psf_m->texture_rgba);
	glGenTextures(1 , &psf_m->texture_name[0]);
    return;
}

void set_texture_to_psf(int img_fmt, const char *img_head, struct psf_menu_val *psf_m) {
	
	if(img_fmt == SAVE_PNG){
		set_texture_from_png(img_head, psf_m);
	}else if (img_fmt == SAVE_BMP){
		set_texture_from_bmp(img_head, psf_m);
	};
	
	return;
}
