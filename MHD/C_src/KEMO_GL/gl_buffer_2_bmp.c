
/* gl_buffer_2_bmp.c */

#include "gl_buffer_2_bmp.h"

static unsigned char *image;

void flip_gl_bitmap(int num_x, int num_y,
                    unsigned char *glimage, unsigned char *fliped_img){
	int i, j, k, l;
	
	for (i = 0; i < num_x; i++) {
		for (j = 0; j < num_y; j++) {
			k = (num_y-j-1)*num_x + i;
			l = j*num_x +i;
			fliped_img[3*l  ] = glimage[3*k];
			fliped_img[3*l+1] = glimage[3*k+1];
			fliped_img[3*l+2] = glimage[3*k+2];
		}
	}
	return;
}

void flip_gl_bitmap_to_img2d(int num_x, int num_y,
                             unsigned char *glimage, unsigned char **img_2d){
	int i, j, k;
	
	for (i = 0; i < num_x; i++) {
		for (j = 0; j < num_y; j++) {
			k = (num_y-j-1)*num_x + i;
			img_2d[j][3*i  ] = glimage[3*k];
			img_2d[j][3*i+1] = glimage[3*k+1];
			img_2d[j][3*i+2] = glimage[3*k+2];
		}
	}
	return;
}

void get_gl_buffer_to_bmp(int num_x, int num_y, unsigned char *glimage){
	glReadBuffer(GL_FRONT);
	glPixelStorei(GL_PACK_ALIGNMENT, IONE);
	glReadPixels(IZERO, IZERO, (GLsizei) num_x, (GLsizei) num_y,
                 GL_RGB, GL_UNSIGNED_BYTE,(GLubyte *) glimage);
}

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
