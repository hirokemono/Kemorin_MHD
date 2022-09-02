
/* read_image_2_bmp.c */

#include "read_image_2_bmp.h"


static FILE *fp_bmp;

static int bit4_2_num(unsigned char *byt4){
	int num;
	
	num = (int) byt4[3];
	num = 256 * num + (int) byt4[2];
	num = 256 * num + (int) byt4[1];
	num = 256 * num + (int) byt4[0];
	return num;
}

void alloc_bgr_data(struct BMP_data *d_BMP){
	d_BMP->npixel = (d_BMP->ihpixf) * (d_BMP->jvpixf);
	d_BMP->bgr = (unsigned char *) malloc( (3*d_BMP->npixel) * sizeof(unsigned char));
	if (d_BMP->bgr == NULL) {
		fclose(fp_bmp);
		exit(2);
	}
	return;
}

struct BMP_data * init_BMP_data(void){
    struct BMP_data *d_BMP = (struct BMP_data *) malloc(sizeof(struct BMP_data));
    if (d_BMP == NULL) {
        printf("malloc error fot BMP_data \n");
        exit(0);
    }
    return d_BMP;
};

void dealloc_BMP_data(struct BMP_data *d_BMP){
	free(d_BMP->bgr);
	free(d_BMP);
		return;
}

struct BMP_data * read_BMP_c(const char *fhead){
	struct BMP_data *d_BMP = init_BMP_data();
	
	char fname[LENGTHBUF];
	unsigned char header[54];
	unsigned char char3[3];
	unsigned char byt4[4];
	int i, j, k;
	int irst;
	
	sprintf(fname, "%s.bmp",fhead);
	fp_bmp = fopen(fname,"rb");
	
	/* read header */
	fread(header,1,54,fp_bmp);
	
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
	
	irst = d_BMP->ihpixf % 4;
	if (irst == 0) {
		for (k=0; k < d_BMP->npixel; k++) {
			/* reading in BGR order, not rgb. */
			fread (&d_BMP->bgr[3*k  ],1,1,fp_bmp);
			fread (&d_BMP->bgr[3*k+1],1,1,fp_bmp);
			fread (&d_BMP->bgr[3*k+2],1,1,fp_bmp);
		};
	} else {
		for (j=0; j < d_BMP->jvpixf; j++) {
			for (i=0; i < d_BMP->ihpixf; i++) {
				k = i + j * 3 * d_BMP->ihpixf;
				fread (&d_BMP->bgr[3*k  ],1,1,fp_bmp);
				fread (&d_BMP->bgr[3*k+1],1,1,fp_bmp);
				fread (&d_BMP->bgr[3*k+2],1,1,fp_bmp);
			};
			for (i=0; i < (4-irst) ; i++){
				fread (&char3[0],1,3,fp_bmp);
			};
		};
	};
	
	fclose(fp_bmp);
	return d_BMP;
}

struct BMP_data * read_gzipped_BMP_c(const char *fhead){
	struct BMP_data *d_BMP = init_BMP_data();
	char gzip_name[LENGTHBUF];
	unsigned char header[54];
	unsigned char byt4[4];
	int iflag_swap = IZERO;
	int ilength;
	int ierr;
	
	sprintf(gzip_name, "%s.bmp.gz",fhead);
    void * FP_gzip1 = open_rd_gzfile_c(gzip_name);
	printf("open_rd_gzfile_c \n");
    ilength = 54;
    gzread_32bit_c(FP_gzip1, &iflag_swap, &ilength, (char *) header, &ierr);
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
    gzread_32bit_c(FP_gzip1, &iflag_swap, &ilength, (char *) d_BMP->bgr, &ierr);
    close_gzfile_c(FP_gzip1);
	/*
	for(ilength=0;d_BMP->npixel<54;ilength++){
		printf("bgr %d %x %x %x\n", ilength, 
			   d_BMP->bgr[3*ilength-2], d_BMP->bgr[3*ilength-1], d_BMP->bgr[3*ilength]);
	}
	*/
	return d_BMP;
};

void copy_rgb_from_BMP_c(struct BMP_data *d_BMP, unsigned char *image){
	int k;
	for (k=0; k < d_BMP->npixel; k++) {
		/* reading in BGR order, not RGB. */
		image[3*k  ] = d_BMP->bgr[3*k+2];
		image[3*k+1] = d_BMP->bgr[3*k+1];
		image[3*k+2] = d_BMP->bgr[3*k  ];
	};
    return;
}

void copy_rgba_from_BMP_c(struct BMP_data *d_BMP, unsigned char *image){
	int k;
	for (k=0; k < d_BMP->npixel; k++) {
		/* reading in BGR order, not RGB. */
		image[4*k  ] = d_BMP->bgr[3*k+2];
		image[4*k+1] = d_BMP->bgr[3*k+1];
		image[4*k+2] = d_BMP->bgr[3*k  ];
		image[4*k+3] = (unsigned char) 255;
	};
    return;
}
