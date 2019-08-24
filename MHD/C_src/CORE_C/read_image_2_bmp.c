
/* read_image_2_bmp.c */

#include "read_image_2_bmp.h"


static FILE *fp_bmp;
unsigned char *bgr;

static int bit4_2_num(unsigned char *byt4){
	int num;
	
	num = (int) byt4[3];
	num = 256 * num + (int) byt4[2];
	num = 256 * num + (int) byt4[1];
	num = 256 * num + (int) byt4[0];
	return num;
}

int read_BMP_c(const char *fhead, int *ihpixf, int *jvpixf){
	char fname[LENGTHBUF];
	unsigned char header[54];
	unsigned char char3[3];
	unsigned char byt4[4];
	int i, j, k, npixel;
	int irst;
	
	sprintf(fname, "%s.bmp",fhead);
	fp_bmp = fopen(fname,"rb");
	
	/* read header */
	fread(header,1,54,fp_bmp);
	
	byt4[0] = header[18];
	byt4[1] = header[19];
	byt4[2] = header[20];
	byt4[3] = header[21];
	*ihpixf = bit4_2_num(byt4);
	
	byt4[0] = header[22];
	byt4[1] = header[23];
	byt4[2] = header[24];
	byt4[3] = header[25];
	*jvpixf = bit4_2_num(byt4);
	
	npixel = (*ihpixf) * (*jvpixf);
	if ((bgr = (unsigned char *) malloc( (3*npixel) * sizeof(unsigned char))) == NULL) {
		fclose(fp_bmp);
		exit(2);
	}
	
	irst = *ihpixf % 4;
	if (irst == 0) {
		for (k=0; k < npixel; k++) {
			/* reading in BGR order, not rgb. */
			fread (&bgr[3*k  ],1,1,fp_bmp);
			fread (&bgr[3*k+1],1,1,fp_bmp);
			fread (&bgr[3*k+2],1,1,fp_bmp);
		};
	} else {
		for (j=0; j < *jvpixf; j++) {
			for (i=0; i < *ihpixf; i++) {
				k = i + j * *ihpixf;
				fread (&bgr[3*k  ],1,1,fp_bmp);
				fread (&bgr[3*k+1],1,1,fp_bmp);
				fread (&bgr[3*k+2],1,1,fp_bmp);
			};
			for (i=0; i < (4-irst) ; i++){
				fread (&char3[0],1,3,fp_bmp);
			};
		};
	};
	
	fclose(fp_bmp);
	return 0;
}

void copy_rgb_from_BMP_c(int ihpixf, int jvpixf, unsigned char *image){
	int k;
	for (k=0; k < ihpixf*jvpixf; k++) {
		/* reading in BGR order, not RGB. */
		image[3*k  ] = bgr[3*k+2];
		image[3*k+1] = bgr[3*k+1];
		image[3*k+2] = bgr[3*k  ];
	};
	free(bgr);
}

void copy_rgba_from_BMP_c(int ihpixf, int jvpixf, unsigned char *image){
	int k;
	for (k=0; k < ihpixf*jvpixf; k++) {
		/* reading in BGR order, not RGB. */
		image[4*k  ] = bgr[3*k+2];
		image[4*k+1] = bgr[3*k+1];
		image[4*k+2] = bgr[3*k  ];
		image[4*k+3] = (unsigned char) 255;
	};
	free(bgr);
}
