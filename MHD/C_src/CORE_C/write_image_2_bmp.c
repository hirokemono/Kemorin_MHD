
/* write_image_2_bmp.c */


#include "write_image_2_bmp.h"

static FILE *fp;


unsigned char * alloc_RGB_buffer_to_bmp(int ihpixf, int jvpixf){
    unsigned char *image;
    if((image = (unsigned char*)calloc(3*ihpixf*jvpixf, sizeof(unsigned char))) == NULL){
        printf("malloc error for Bitmap image buffer \n");
        exit(0);
    };
    return image;
}


void cvt_8bit_cl_int_2_chara(int ihpixf, int jvpixf, int *icl_tbl, unsigned char *rgb){ 
	int i, j, k;
	
	for (j=0; j < jvpixf; j++) {
		for (i=0; i < ihpixf; i++) {
			k = i + j * ihpixf;
			rgb[3*k  ] = (unsigned char) icl_tbl[3*k  ];
			rgb[3*k+1] = (unsigned char) icl_tbl[3*k+1];
			rgb[3*k+2] = (unsigned char) icl_tbl[3*k+2];
		};
	};
	return;
};

/* --------------------------------------*/
/* convert number to 8-bit characters    */
/* --------------------------------------*/

static void num2bit2(int inum, char *byt2){
	int itmp1, itmp2;
	
	itmp1 = inum;
	itmp2 = itmp1 / 256;
	byt2[1] =  (char) itmp2;
	itmp1 =-itmp2 * 256 + itmp1;
	byt2[0] =  (char) itmp1;
	return;
};

static void num2bit4(int inum, char *byt4){
	int itmp1, itmp2;
	
	itmp1 = inum;
	itmp2 = itmp1 / (256*256*256);
	byt4[3] = (char) itmp2;
	itmp1 =-itmp2 * (256*256*256) +itmp1;
	itmp2 = itmp1 / (256*256);
	byt4[2] = (char) itmp2;
	itmp1 =-itmp2 * (256*256) +itmp1;
	itmp2 = itmp1 / 256;
	byt4[1] = (char) itmp2;
	itmp1 =-itmp2 * 256    +itmp1;
	byt4[0] = (char) itmp1;
	return;
}


int pixout_BMP_c(const char *fhead, int ihpixf, int jvpixf,
			const unsigned char *rgb){
	char fname[LENGTHBUF];
	
/* RGB data array
       character(len=1), intent(in) :: rgb[3*ihpixf*jvpixf) */
/* local */
	int i, j, k;
	int itmp, irst;
	
	char headmsw[55];
	char byt4[4];
	char byt2[2];
	
	sprintf(fname, "%s.bmp",fhead);
	/*printf("BMP file name: %s \n",fname);*/
/* BMP (24bit depth)... this part works only when width is multiple of 4. */
	
	itmp = ihpixf%4;
	if (itmp != 0) {
		printf("width must be multiple of 4 \n" );
		return 1;
	};
	if ((fp = fopen(fname, "w")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", fname);
		exit (2);                    /* terminate with error message */
	}
	
	printf("Now writing BMP(24bit) file : %s \n", fname);
	
	/* header 1 (file header ; 1--14 byte) */
	headmsw[0] = 'B';
	headmsw[1] = 'M';     /* declaring this is BMP file */
	
	itmp = 54 + ihpixf * jvpixf * 3;   /* total file size = header + data */
	num2bit4(itmp, byt4);
	
	headmsw[2] = byt4[0];
	headmsw[3] = byt4[1];
	headmsw[4] = byt4[2];
	headmsw[5] = byt4[3];
	
	itmp = 0;                        /* may be 0 */
	num2bit2(itmp,byt2);
	headmsw[6] = byt2[0];
	headmsw[7] = byt2[1];
	
	itmp = 0;                        /* may be 0 */
	num2bit2(itmp,byt2);
	headmsw[8] = byt2[0];
	headmsw[9] = byt2[1];
	
	itmp = 54;                       /* must be 54 : total length of header */
	num2bit4(itmp,byt4);
	
	headmsw[10] = byt4[0];
	headmsw[11] = byt4[1];
	headmsw[12] = byt4[2];
	headmsw[13] = byt4[3];
	
	/* header 2 (bit-map header ; 13--54 byte) */
	itmp = 40;                       /* must be 40 : length of bit-map header */
	num2bit4(itmp,byt4);
	
	headmsw[14] = byt4[0];
	headmsw[15] = byt4[1];
	headmsw[16] = byt4[2];
	headmsw[17] = byt4[3];
	
	itmp = ihpixf;                   /* width */
	num2bit4(itmp,byt4);
	
	headmsw[18] = byt4[0];
	headmsw[19] = byt4[1];
	headmsw[20] = byt4[2];
	headmsw[21] = byt4[3];
	
	itmp = jvpixf;                   /* height */
	num2bit4(itmp,byt4);
	
	headmsw[22] = byt4[0];
	headmsw[23] = byt4[1];
	headmsw[24] = byt4[2];
	headmsw[25] = byt4[3];
	
	itmp = 1;                        /* must be 1 */
	num2bit2(itmp,byt2);
	headmsw[26] = byt2[0];
	headmsw[27] = byt2[1];
	
	itmp = 24;                       /* must be 24 : color depth in bit. */
	num2bit2(itmp,byt2);
	headmsw[28] = byt2[0];
	headmsw[29] = byt2[1];
	
	itmp = 0;                        /* may be 0 : compression method index */
	num2bit4(itmp,byt4);
	
	headmsw[30] = byt4[0];
	headmsw[31] = byt4[1];
	headmsw[32] = byt4[2];
	headmsw[33] = byt4[3];
	
	itmp = 0;                        /* may be 0 : file size if compressed */
	num2bit4(itmp,byt4);
	
	headmsw[34] = byt4[0];
	headmsw[35] = byt4[1];
	headmsw[36] = byt4[2];
	headmsw[37] = byt4[3];
	
	itmp = 0;                        /* arbit. : pixel per meter, horizontal */
	num2bit4(itmp,byt4);
	
	headmsw[38] = byt4[0];
	headmsw[39] = byt4[1];
	headmsw[40] = byt4[2];
	headmsw[41] = byt4[3];
	
	itmp = 0;                        /* arbit. : pixel per meter, vertical */
	num2bit4(itmp,byt4);
	
	headmsw[42] = byt4[0];
	headmsw[43] = byt4[1];
	headmsw[44] = byt4[2];
	headmsw[45] = byt4[3];
	
	itmp = 0;                        /* may be 0 here : num. of color used */
	num2bit4(itmp,byt4);
	
	headmsw[46] = byt4[0];
	headmsw[47] = byt4[1];
	headmsw[48] = byt4[2];
	headmsw[49] = byt4[3];
	
	itmp = 0;                        /* may be 0 here : num. of important color */
	num2bit4(itmp,byt4);
	
	headmsw[50] = byt4[0];
	headmsw[51] = byt4[1];
	headmsw[52] = byt4[2];
	headmsw[53] = byt4[3];
	
	headmsw[54] ='\0';
	
	/* writing header part */
	for (k=0; k < 54; k++) {
		fprintf (fp, "%c", headmsw[k]);
	};
	
	/* image data */
	/*itmp = ihpixf * jvpixf;*/
	irst = ihpixf % 4;
	if (irst == 0) {
		for (k=0; k < jvpixf*ihpixf; k++) {
			/* writing in BGR order, not RGB. */
			fprintf (fp, "%c", rgb[3*k+2]);
			fprintf (fp, "%c", rgb[3*k+1]);
			fprintf (fp, "%c", rgb[3*k  ]);
		};
	} else {
		for (j=0; j < jvpixf; j++) {
			for (i=0; i < ihpixf; i++) {
				k = i + j * ihpixf;
				fprintf (fp, "%c", rgb[3*k+2]);
				fprintf (fp, "%c", rgb[3*k+1]);
				fprintf (fp, "%c", rgb[3*k  ]);
			};
			for (i=0; i < (4-irst) ; i++){
				fprintf (fp, "%c", (char) 0);
				fprintf (fp, "%c", (char) 0);
				fprintf (fp, "%c", (char) 0);
			};
		};
	};
	fclose(fp);                                /* close file */
	return 0;
}


int pixout_ppm_p6_c(const char *fhead, int ihpixf, int jvpixf,
			const unsigned char *rgb) {
	char fname[LENGTHBUF];
	int i, j, k;
	
	sprintf(fname, "%s.ppm",fhead);
	printf("PPM(P6) file name: %s \n",fname);
	/* PPM P6 */
	if ((fp = fopen(fname, "w")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", fname);
		exit (2);                    /* terminate with error message */
	}
	
	printf("Now writing PPM(P6) file : %s \n", fname);
	
	/* header */
	fprintf (fp, "P6 %4d %4d 255 ", ihpixf, jvpixf);
	/* image data */
	/* ! some compiler may not accept this line.
	! here, j (vertical address) runs from top to bottom. */
	for (j=0; j < jvpixf; j++) {
		for (i=0; i < ihpixf; i++) {
			k = i + (jvpixf-j-1) * ihpixf;
			fprintf (fp, "%c", rgb[3*k  ]);
			fprintf (fp, "%c", rgb[3*k+1]);
			fprintf (fp, "%c", rgb[3*k+2]);
		};
	};
	fclose(fp);                                /* close file */
	return 0;

}

int pixout_ppm_p3_c(const char *fhead, int ihpixf, int jvpixf,
			const unsigned char *rgb) {
	char fname[LENGTHBUF];
	int i, j, k, nd;
	int icnt;
	unsigned char itmp;
	
	sprintf(fname, "%s.ppm",fhead);
	printf("PPM(P3) file name: %s \n",fname);
	/* PPM P6 */
	if ((fp = fopen(fname, "w")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", fname);
		exit (2);                    /* terminate with error message */
	}
	
	printf("Now writing PPM(P3) file : %s \n", fname);
	
	/* header */
	fprintf (fp, "P3\n");
	fprintf (fp, "%4d %4d 255 \n", ihpixf, jvpixf);
	/* image data */
	/* ! some compiler may not accept this line.
	! here, j (vertical address) runs from top to bottom. */
	/*itmp = ihpixf * jvpixf * 3;*/
	icnt = 0;
	for (j=0; j < jvpixf; j++) {
		for (i=0; i < ihpixf; i++) {
			k = i + (jvpixf-j-1) * ihpixf;
			for (nd = 0; nd < 3; nd++) { 
				itmp = (unsigned int) rgb[3*k+nd];
				icnt = icnt + 1;
				if (icnt < 60) {
					fprintf (fp, "%4u", itmp);
				} else {
					fprintf (fp, "%4d\n", itmp);
				};
			};
		};
	};
	fprintf (fp, "\n");
	fclose(fp);                                /* close file */
	return 0;

}
