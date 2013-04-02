
/* gl_buffer_2_bmp.c */

#include "gl_buffer_2_bmp.h"

static unsigned char *image;
static GLubyte *pixels_gl;

static void get_gl_buffer_4_bmp(int num_x, int num_y){
	int i;
	GLsizei width_gl, height_gl;
	
	width_gl =  num_x;
	height_gl = num_y;
	
	pixels_gl = (GLubyte *)calloc(3*width_gl*height_gl,sizeof(GLubyte));
		
	glReadBuffer(GL_FRONT);
	glPixelStorei(GL_PACK_ALIGNMENT, IONE);
	glReadPixels(IZERO, IZERO, width_gl, height_gl, GL_RGB, GL_UNSIGNED_BYTE, pixels_gl );
	
	for (i = 0; i < 3*num_x*num_y;i++) { image[i] = pixels_gl[i]; };
	free(pixels_gl);
}


void gl_buffer_to_bmp(const char *fhead, int num_x, int num_y){
	
	image = (unsigned char*)calloc(3*num_x*num_y, sizeof(unsigned char));
	get_gl_buffer_4_bmp(num_x, num_y);
	
	pixout_BMP_c(fhead, num_x, num_y, image);
	free(image);
	return;
}

void gl_buffer_to_ppm_p6(const char *fhead, int num_x, int num_y){
	
	image = (unsigned char*)calloc(3*num_x*num_y, sizeof(unsigned char));
	get_gl_buffer_4_bmp(num_x, num_y);
	
	pixout_ppm_p6_c(fhead, num_x, num_y, image);
	free(image);
	return;
}

void gl_buffer_to_ppm_p3(const char *fhead, int num_x, int num_y){
	
	image = (unsigned char*)calloc(3*num_x*num_y, sizeof(unsigned char));
	get_gl_buffer_4_bmp(num_x, num_y);
	
	pixout_ppm_p3_c(fhead, num_x, num_y, image);
	free(image);
	return;
}
