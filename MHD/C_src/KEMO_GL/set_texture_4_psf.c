
/* set_texture_4_psf.c */

#include "set_texture_4_psf.h"

void set_texture_4_psf(int width, int height, const unsigned char *bgra_in, 
			struct psf_menu_val *psf_m) {
	
	psf_m->texture_width =  width;
	psf_m->texture_height = height;
	
	alloc_draw_psf_texture(psf_m);
	vart_flip_rgba_c(psf_m->texture_width, psf_m->texture_height, bgra_in, 
			psf_m->texture_rgba);
	glGenTextures(1 , &psf_m->texture_name[0]);
	
	return;
}

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

int set_texture_from_file(int img_fmt, const char *img_head, struct psf_menu_val *psf_m) {
	int  iflag_rgba;
	
	if(img_fmt == SAVE_PNG){
		set_texture_from_png(img_head, psf_m);
	}else if (img_fmt == SAVE_BMP){
		set_texture_from_bmp(img_head, psf_m);
	} else {
		return 99;
	};
	
	return img_fmt;
}

void release_texture_4_psf(struct psf_menu_val *psf_m) {
	glDeleteTextures(1, &psf_m->texture_name[0]);
	dealloc_draw_psf_texture(psf_m);
	return;
}
