
/* set_texture_4_psf.c */

#include "set_texture_4_psf.h"

int set_texture_4_psf(int img_fmt, const char *img_head, struct psf_menu_val *psf_m) {
	int  iflag_rgba;
	
	if(img_fmt == SAVE_PNG){
		read_png_file_c(img_head, &psf_m->texture_width, &psf_m->texture_height, &iflag_rgba);
		alloc_draw_psf_texture(psf_m);
		copy_rgba_from_png_c(&psf_m->texture_width, &psf_m->texture_height, 
				&iflag_rgba, psf_m->texture_rgba);
	}else if (img_fmt == SAVE_BMP){
		read_BMP_c(img_head, &psf_m->texture_width, &psf_m->texture_height);
		alloc_draw_psf_texture(psf_m);
		copy_rgba_from_BMP_c(psf_m->texture_width, psf_m->texture_height, 
				psf_m->texture_rgba);
	} else {
		return 99;
	};
	
	glGenTextures(1 , &psf_m->texture_name[0]);
	
	return img_fmt;
}

void release_texture_4_psf(struct psf_menu_val *psf_m) {
	glDeleteTextures(1, &psf_m->texture_name[0]);
	dealloc_draw_psf_texture(psf_m);
	return;
}
