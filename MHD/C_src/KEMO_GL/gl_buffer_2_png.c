
/* gl_buffer_2_png.c */

#include "gl_buffer_2_png.h"

#define _SPACE 0x20

static unsigned char **image;
static GLubyte *glimage;

void alloc_img_buffer_2_png_rgba(int num_x, int num_y){
	int j;
	/* allocate memory image[y_pixel#][4*x_pixel#]*/
	image = (png_bytepp)malloc(num_y * sizeof(png_bytep));
	for (j = 0; j < num_y; j++)
	        image[j] = (png_bytep)malloc(4*num_x * sizeof(png_byte));
};

void alloc_img_buffer_2_png_rgb(int num_x, int num_y){
	int j;
	/* allocate memory image[y_pixel#][3*x_pixel#]*/
	image = (png_bytepp)malloc(num_y * sizeof(png_bytep));
	for (j = 0; j < num_y; j++)
	        image[j] = (png_bytep)malloc(3*num_x * sizeof(png_byte));
};

void link_img_buffer_4_png(unsigned char **image_p){
	image_p = image;
	return;
};

void dealloc_img_buffer_2_png(int num_y){
	int j;
	for (j = 0; j < num_y; j++) free(image[j]);
	free(image);
};

void get_gl_buffer_kemo(int num_x, int num_y){
	int i, j, k;
	GLsizei  width_gl, height_gl;
	
	glimage = malloc(num_x*num_y*3 * sizeof(GLubyte));
	width_gl =  num_x;
	height_gl = num_y;
	
	glReadBuffer(GL_FRONT);
	glPixelStorei(GL_PACK_ALIGNMENT, IONE);
	glReadPixels(IZERO, IZERO, width_gl, height_gl, GL_RGB, GL_UNSIGNED_BYTE, glimage);
	
	for (i = 0; i < num_x; i++) {
		for (j = 0; j < num_y; j++) {
			k = (num_y-j-1)*num_x + i;
			image[j][3*i  ] = glimage[3*k];
			image[j][3*i+1] = glimage[3*k+1];
			image[j][3*i+2] = glimage[3*k+2];
		}
	}
	
	free(glimage);
	return;
}

void gl_buffer_2_png(const char *fhead, int num_x, int num_y){
	char fname[LENGTHBUF];
	
	/* allocate memory */
	alloc_img_buffer_2_png_rgb(num_x, num_y);
	
	get_gl_buffer_kemo(num_x, num_y);
	
	sprintf(fname, "%s.png",fhead);
	printf("PNG file name: %s \n",fname);
	write_png_rgb(fname,(unsigned long) num_x,(unsigned long) num_y, image);
	
	/* deallocate memory*/
	dealloc_img_buffer_2_png(num_y);
	return;
}
