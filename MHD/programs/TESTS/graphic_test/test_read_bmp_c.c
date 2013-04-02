
/* kemoviewer_glut.c */


#include <stdio.h>
#include "read_image_2_bmp.h"
#include "write_image_2_png.h"
#include "write_image_2_bmp.h"

int main(int argc, char *argv[])
{
	int width, height;
	unsigned char *image;
	int i;
	
	if(argc < 3){
		return 1;
		printf("%Command format is \n");
		printf("%test_read_bmp [input file header] [output file header]\n");
	};
	
	read_BMP_c(argv[1], &width, &height);
	if ((image = (unsigned char *) malloc( (3*width*height) * sizeof(unsigned char))) == NULL) {
		exit(2);
	}
	copy_rgb_from_BMP_c(width, height, image);
	
	pixout_BMP_c(argv[2], width, height, image);
	write_png_rgb_c(argv[2], &width, &height, image);
	
	return 1;
}
