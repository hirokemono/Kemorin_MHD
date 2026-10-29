
/* read_gzipped_BMP_image.c */

#include "read_gzipped_BMP_image.h"

struct BMP_data * read_gzipped_BMP_c(const char *fhead){
	struct BMP_data *d_BMP = init_BMP_data();
	char gzip_name[LENGTHBUF];
	unsigned char header[54];
	unsigned char byt4[4];
	int iflag_swap = IZERO;
	int ilength;
	
	sprintf(gzip_name, "%s.bmp.gz",fhead);
    void * FP_gzip1 = open_rd_gzfile_c(gzip_name);
	printf("open_rd_gzfile_c \n");
    
    int ierr = gzread_32bit_c(FP_gzip1, iflag_swap, 54, (char *) header);
	/*
	for(ilength=0;ilength<54;ilength++){
		printf("header %d %x\n", ilength, header[ilength]);
	}
	*/
	byt4[0] = header[18];
	byt4[1] = header[19];
	byt4[2] = header[20];
	byt4[3] = header[21];
	d_BMP->ihpixf = bit4_2_num(byt4);
	
	byt4[0] = header[22];
	byt4[1] = header[23];
	byt4[2] = header[24];
	byt4[3] = header[25];
	d_BMP->jvpixf = bit4_2_num(byt4);
	alloc_bgr_data(d_BMP);
	
    ilength = 3*d_BMP->npixel;
    ierr = gzread_32bit_c(FP_gzip1, iflag_swap, ilength, (char *) d_BMP->bgr);
    close_gzfile_c(FP_gzip1);
	/*
	for(ilength=0;d_BMP->npixel<54;ilength++){
		printf("bgr %d %x %x %x\n", ilength, 
			   d_BMP->bgr[3*ilength-2], d_BMP->bgr[3*ilength-1], d_BMP->bgr[3*ilength]);
	}
	*/
	return d_BMP;
};
