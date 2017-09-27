
/* gl_buffer_2_png.c */

#include "gl_buffer_2_png.h"

#define _SPACE 0x20

static unsigned char **image;

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

static void get_gl_buffer_for_png(int num_x, int num_y){
    unsigned char *glimage;
	
	glimage = malloc(num_x*num_y*3 * sizeof(unsigned char));
	get_gl_buffer_to_bmp(num_x, num_y, glimage);
    flip_gl_bitmap_to_img2d(num_x, num_y, glimage, image);
	free(glimage);
	return;
}

void write_gl_window_to_png(const char *fhead, int num_x, int num_y){
	char fname[LENGTHBUF];
	
	/* allocate memory */
	alloc_img_buffer_2_png_rgb(num_x, num_y);
	
	get_gl_buffer_for_png(num_x, num_y);
	
	sprintf(fname, "%s.png",fhead);
	printf("PNG file name: %s \n",fname);
	write_png_rgb(fname,(unsigned long) num_x,(unsigned long) num_y, image);
	
	/* deallocate memory*/
	dealloc_img_buffer_2_png(num_y);
	return;
}

void write_gl_window_step_png(int istep, const char *fhead,
                               GLint nwin_x, GLint nwin_y){
    char fhead_step[LENGTHBUF];
    
    sprintf(fhead_step, "%s.%d%c",fhead, istep,'\0');
    /* printf("file header: %s \n",fhead_step); */
    write_gl_window_to_png(fhead_step, nwin_x, nwin_y);
    return;
}
