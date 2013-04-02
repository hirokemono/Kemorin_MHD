
/* kemoviewer_glut.c */

#include <stdio.h>

#include "read_image_2_png.h"
#include "write_image_2_png.h"
#include "write_image_2_bmp.h"

int main(int argc, char *argv[])
{
	int   width, height;
	int iflag_rgba;
	unsigned char   *cimage;
	int i;
	
	if(argc < 3){
		printf("Command format is \n");
		printf("test_read_png [input file header] [output file header]\n");
		return 1;
	};
	
	read_png_file_c(argv[1], &width, &height, &iflag_rgba);
	if ((cimage = (unsigned char *) malloc( (3*width*height) * sizeof(unsigned char))) == NULL) {
		exit(2);
	}
	printf("iflag_rgba %d\n", iflag_rgba);
	copy_rgb_from_png_c(&width, &height, &iflag_rgba, cimage);
	
	pixout_BMP_c(argv[2], width, height, cimage);
	write_png_rgb_c(argv[2], &width, &height, cimage);
	free(cimage);
	return 0;
}

