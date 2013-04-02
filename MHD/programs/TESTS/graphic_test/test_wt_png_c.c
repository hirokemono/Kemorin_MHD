#include <stdio.h>
#include <stdlib.h>
#include "png.h"
#include "write_image_2_png.h"

#define WIDTH  (256)
#define HEIGHT (128)



void set_image1_RGBA(int num_x, int num_y, png_bytepp image);
void set_image2_RGBA(int num_x, int num_y, png_bytepp image);

void set_image1_RGB(int num_x, int num_y, png_bytepp image);
void set_image2_RGB(int num_x, int num_y, png_bytepp image);


int test_PNG_RGBA(char *file_name1, char *file_name2)
{
	int             j;
	unsigned char   **image;
	
	/* allocate memory */
	image = (png_bytepp)malloc(HEIGHT * sizeof(png_bytep));
	for (j = 0; j < HEIGHT; j++)
	        image[j] = (png_bytep)malloc(4*WIDTH * sizeof(png_byte));
	
	set_image1_RGBA(WIDTH, HEIGHT, image);
	write_png_rgba(file_name1, WIDTH, HEIGHT, image);
	set_image2_RGBA(WIDTH, HEIGHT, image);
	write_png_rgba(file_name2, WIDTH, HEIGHT, image);
	
	/* deallocate memory */
	for (j = 0; j < HEIGHT; j++) free(image[j]);
	free(image);
	
	return 0;
}

int test_PNG_RGB(char *file_name1, char *file_name2)
{
	int             j;
	unsigned char   **image;
	
	/* allocate memory */
	image = (png_bytepp)malloc(HEIGHT * sizeof(png_bytep));
	for (j = 0; j < HEIGHT; j++)
	        image[j] = (png_bytep)malloc(3*WIDTH * sizeof(png_byte));
	
	set_image1_RGB(WIDTH, HEIGHT, image);
	write_png_rgb(file_name1, WIDTH, HEIGHT, image);
	set_image2_RGB(WIDTH, HEIGHT, image);
	write_png_rgb(file_name2, WIDTH, HEIGHT, image);
	
	/* deallocate memory */
	for (j = 0; j < HEIGHT; j++) free(image[j]);
	free(image);
	
	return 0;
}

void set_image1_RGBA(int num_x, int num_y, png_bytepp image)
{
	int             i, j;
	
	/* test pattern for Red*/
	for (i = 0; i < num_x; i++) {
	        for (j = 0; j < 40; j++) {
	                image[j][4*i  ] = (unsigned char)i;
	                image[j][4*i+1] = (unsigned char)0;
	                image[j][4*i+2] = (unsigned char)0;
	                image[j][4*i+3] = (unsigned char)255;
	        }
	}
	
	/* test pattern for Green*/
	for (i = 0; i < num_x; i++) {
	        for (j = 40; j < 80; j++) {
	                image[j][4*i] = (unsigned char)0  ;
	                image[j][4*i+1] = (unsigned char)num_x-i-1;
	                image[j][4*i+2] = (unsigned char)0;
	                image[j][4*i+3] = (unsigned char)128;
	        }
	}
	
	/* test pattern for blue*/
	for (i = 0; i < num_x; i++) {
	        for (j = 80; j < num_y; j++) {
	                image[j][4*i  ] = (unsigned char)0;
	                image[j][4*i+1] = (unsigned char)0;
	                image[j][4*i+2] = (unsigned char)i;
	                image[j][4*i+3] = (unsigned char)num_x-i-1;
	        }
	}
	return;
}

void set_image2_RGBA(int num_x, int num_y, png_bytepp image)
{
	int             i, j;
	
	for (i = 0; i < num_x; i++) {
	        for (j = 0; j < 40; j++) {
	                image[j][4*i  ] = (unsigned char)i;
	                image[j][4*i+1] = (unsigned char)255;
	                image[j][4*i+2] = (unsigned char)255;
	                image[j][4*i+3] = (unsigned char)255;
	        }
	}
	
	/* test pattern for Green*/
	for (i = 0; i < num_x; i++) {
	        for (j = 40; j < 80; j++) {
	                image[j][4*i  ] = (unsigned char)255;
	                image[j][4*i+1] = (unsigned char)num_x-i-1;
	                image[j][4*i+2] = (unsigned char)255;
	                image[j][4*i+3] = (unsigned char)num_x-i-1;
		}
	}
	
	/* test pattern for blue*/
	for (i = 0; i < num_x; i++) {
	        for (j = 80; j < num_y; j++) {
	                image[j][4*i  ] = (unsigned char)255;
	                image[j][4*i+1] = (unsigned char)255;
	                image[j][4*i+2] = (unsigned char)i;
	                image[j][4*i+3] = (unsigned char)128;
	        }
	}
	return;
}


void set_image1_RGB(int num_x, int num_y, png_bytepp image)
{
	int             i, j;
	
	for (i = 0; i < num_x; i++) {
	        for (j = 0; j < 40; j++) {
	                image[j][3*i  ] = (unsigned char)i;
	                image[j][3*i+1] = (unsigned char)0;
	                image[j][3*i+2] = (unsigned char)0;
	        }
	}
	
	/* test pattern for Green*/
	for (i = 0; i < num_x; i++) {
	        for (j = 40; j < 80; j++) {
	                image[j][3*i] = (unsigned char)0  ;
	                image[j][3*i+1] = (unsigned char)num_x-i-1;
	                image[j][3*i+2] = (unsigned char)0;
	        }
	}
	
	/* test pattern for blue*/
	for (i = 0; i < num_x; i++) {
	        for (j = 80; j < num_y; j++) {
	                image[j][3*i  ] = (unsigned char)0;
	                image[j][3*i+1] = (unsigned char)0;
	                image[j][3*i+2] = (unsigned char)i;
	        }
	}
	return;
}

void set_image2_RGB(int num_x, int num_y, png_bytepp image)
{
	int             i, j;
	
	for (i = 0; i < num_x; i++) {
	        for (j = 0; j < 40; j++) {
	                image[j][3*i  ] = (unsigned char)i;
	                image[j][3*i+1] = (unsigned char)255;
	                image[j][3*i+2] = (unsigned char)255;
	        }
	}
	
	/* test pattern for Green*/
	for (i = 0; i < num_x; i++) {
	        for (j = 40; j < 80; j++) {
	                image[j][3*i  ] = (unsigned char)255;
	                image[j][3*i+1] = (unsigned char)num_x-i-1;
	                image[j][3*i+2] = (unsigned char)255;
		}
	}
	
	/* test pattern for blue*/
	for (i = 0; i < num_x; i++) {
	        for (j = 80; j < num_y; j++) {
	                image[j][3*i  ] = (unsigned char)255;
	                image[j][3*i+1] = (unsigned char)255;
	                image[j][3*i+2] = (unsigned char)i;
	        }
	}
	return;
}

int main()
{
	test_PNG_RGBA("test1.png", "test2.png");
	test_PNG_RGB("test3.png",  "test4.png");
	return 0;
}
