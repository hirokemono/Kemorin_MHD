
/* gl_buffer_2_png.c */

#include "gl_buffer_2_png.h"

#define _SPACE 0x20

unsigned char ** alloc_img_buffer_2_png_rgba(int num_x, int num_y){
	static unsigned char **image;
	int j;
	/* allocate memory image[y_pixel#][4*x_pixel#]*/
	if((image = (png_bytepp)malloc(num_y * sizeof(png_bytep))) == NULL){
		printf("malloc error for Vertical PNG image buffer \n");
		exit(0);
	};
	for (j = 0; j < num_y; j++){
		if((image[j] = (png_bytep)malloc(4*num_x * sizeof(png_byte))) == NULL){
			printf("malloc error for Horizontal PNG image buffer %d \n", j);
			exit(0);
		};
	};
	return image;
};

unsigned char ** alloc_img_buffer_2_png_rgb(int num_x, int num_y){
	static unsigned char **image;
	int j;
	/* allocate memory image[y_pixel#][3*x_pixel#]*/
	if((image = (png_bytepp)malloc(num_y * sizeof(png_bytep))) == NULL){
		printf("malloc error for Vertical PNG image buffer \n");
		exit(0);
	};
	for (j = 0; j < num_y; j++){
		if((image[j] = (png_bytep)malloc(3*num_x * sizeof(png_byte))) == NULL){
			printf("malloc error for Horizontal PNG image buffer %d \n", j);
			exit(0);
		};
	};
	return image;
};

void dealloc_img_buffer_2_png(int num_y, unsigned char **image){
	int j;
	for (j = 0; j < num_y; j++) free(image[j]);
	free(image);
};

void get_gl_buffer_for_png(int num_x, int num_y, unsigned char **image){
    unsigned char *glimage;
	
	glimage = malloc(num_x*num_y*3 * sizeof(unsigned char));
	get_gl_buffer_to_bmp(num_x, num_y, glimage);
    flip_gl_bitmap_to_img2d(num_x, num_y, glimage, image);
	free(glimage);
	return;
}

void gl_buffer_2_png(const char *fhead, const int num_x, const int num_y,
                     unsigned char **image){
	char fname[LENGTHBUF];
	sprintf(fname, "%s.png",fhead);
	printf("PNG file name: %s \n",fname);
	write_png_rgb(fname,(png_uint_32) num_x,(png_uint_32) num_y, image);
    return;
}
