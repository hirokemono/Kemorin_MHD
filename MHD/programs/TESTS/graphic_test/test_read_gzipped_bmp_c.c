
/* kemoviewer_glut.c */


#include <stdio.h>
#include "read_image_2_bmp.h"
#include "write_image_2_png.h"
#include "write_image_2_bmp.h"

int main(int argc, char *argv[])
{
	unsigned char *image;
	if(argc < 3){
		return 1;
		printf("Command format is \n");
		printf("test_read_bmp [input file header] [output file header]\n");
	};
	
	struct BMP_data *d_BMP = read_gzipped_BMP_c(argv[1]);
    int width =  d_BMP->ihpixf;
    int height = d_BMP->jvpixf;
    
	if ((image = (unsigned char *) malloc( (3*width*height) * sizeof(unsigned char))) == NULL) {
		exit(2);
	}
	copy_rgb_from_BMP_c(d_BMP, image);
	dealloc_BMP_data(d_BMP);
	
	pixout_BMP_c(argv[2], width, height, image);
	write_png_rgb_c(argv[2], &width, &height, (char *) image);
	
	return 1;
}
