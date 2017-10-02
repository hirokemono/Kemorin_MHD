
/* gl_buffer_2_bmp.c */

#include "gl_buffer_2_bmp.h"

static unsigned char *image;


void gl_buffer_to_bmp(const char *fhead, int num_x, int num_y){
	
	image = (unsigned char*)calloc(3*num_x*num_y, sizeof(unsigned char));
	get_gl_buffer_to_bmp(num_x, num_y, image);
	
	pixout_BMP_c(fhead, num_x, num_y, image);
	free(image);
	return;
}

void gl_buffer_to_ppm_p6(const char *fhead, int num_x, int num_y){
	
	image = (unsigned char*)calloc(3*num_x*num_y, sizeof(unsigned char));
	get_gl_buffer_to_bmp(num_x, num_y, image);
	
	pixout_ppm_p6_c(fhead, num_x, num_y, image);
	free(image);
	return;
}

void gl_buffer_to_ppm_p3(const char *fhead, int num_x, int num_y){
	
	image = (unsigned char*)calloc(3*num_x*num_y, sizeof(unsigned char));
	get_gl_buffer_to_bmp(num_x, num_y, image);
	
	pixout_ppm_p3_c(fhead, num_x, num_y, image);
	free(image);
	return;
}
