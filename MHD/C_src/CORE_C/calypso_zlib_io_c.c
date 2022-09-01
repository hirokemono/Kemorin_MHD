/*********************************************************************
    calypso_zlib_io_c.c
    fortran wrapper for zlib IO
*********************************************************************/

#include <string.h>
#include "calypso_zlib_io_c.h"

gzFile file_gzz;

/* The following macro calls a zlib routine and checks the return
 value. If the return value ("status") is not OK, it prints an error
 message and exits the program. Zlib's error statuses are all less
 than zero. */

static gzFile malloc_gzFile(){
	gzFile file_gz = (gzFile) malloc(sizeof(gzFile));
	if(file_gz == NULL){
		printf("malloc error for gzFile structure\n");
		exit(1);
	};
	return file_gz;
}

void * open_wt_gzfile_c(const char *gz_file_name){
	gzFile file_gz = malloc_gzFile();
	if ((file_gz = gzopen(gz_file_name, GZ_WT_MODE)) == NULL){
		fprintf(stderr, "failed to gzopen\n");
		exit(1);
	}
    return (void *) file_gz;
}

void * open_ad_gzfile_c(const char *gz_file_name){
	gzFile file_gz = malloc_gzFile();
	if ((file_gz = gzopen(gz_file_name, GZ_AD_MODE)) == NULL){
		fprintf(stderr, "failed to gzopen\n");
		exit(1);
	}
    return (void *) file_gz;
}
void * open_rd_gzfile_c(const char *gz_file_name){
	gzFile file_gz = malloc_gzFile();
	if ((file_gz = gzopen(gz_file_name, GZ_RD_MODE)) == NULL){
		fprintf(stderr, "failed to gzopen\n");
		exit(1);
	}
    return (void *) file_gz;
}

void close_gzfile_c(void *FP_z){
	int iret;
	if ((iret = gzclose((gzFile) FP_z)) != Z_OK){
		fprintf(stderr, "gzclose failed.\n");
		exit(1);
	}
	return;
}

int open_rd_gzfile_w_flag_c(const char *gz_file_name){
	file_gzz = gzopen(gz_file_name, GZ_RD_MODE);
	if (file_gzz == NULL){
		fprintf(stderr, "failed to gzopen\n");
		return 1;
	}
	return 0;
}

int rewind_gzfile_c(void){
    return gzrewind(file_gzz);
}

int check_gzfile_eof_c(void){
    return gzeof(file_gzz);
}

void write_compress_txt_c(void *FP_z, int *nchara, char *input_txt){
	int writelen, num_txt;
	
	num_txt = (int) strlen(input_txt);
	input_txt[num_txt] =   '\n';
	input_txt[num_txt+1] = '\0';
	num_txt = num_txt + 1;
	/*
	fprintf(stderr,"nchara: %d, num_txt %d,\n",
			*nchara, num_txt);
	*/
	writelen = gzwrite((gzFile) FP_z, input_txt, num_txt);
	if (writelen != num_txt) {
		fprintf(stderr, "failed to gzwrite\n");
		exit(1);
	}
	
	/*memset(input_txt, '\0', *nchara*sizeof(char));*/
	return;
}

void write_compress_txt_nolf_c(int *nchara, char *input_txt){
	int writelen, num_txt;
	
	num_txt = (int) strlen(input_txt);
	input_txt[num_txt] = '\0';
	writelen = gzwrite(file_gzz, input_txt, num_txt);
	if (writelen != num_txt) {
		fprintf(stderr, "failed to gzwrite\n");
		exit(1);
	}
	
	memset(input_txt, '\0', sizeof(*nchara));
	return;
}



static int count_linechara(int num_buffer, const char *line_buf){
	int nchara_l;
	int j;
	
	nchara_l = 0;
	for (j = 0; j < num_buffer; j++) {
		if(line_buf[j] == '\n') {
			nchara_l = j + 1;
			break;
		};
	};
	return nchara_l;
}

/*
static int find_nullpoint(int num_buffer, const char *line_buf){
	int nchara_l;
	int j;
	
	nchara_l = 0;
	for (j = 0; j < num_buffer; j++) {
		if(line_buf[j] == '\n') {
			nchara_l = j + 1;
			break;
		};
	};
	return nchara_l;
}
*/

static int count_words(int nchara_l, const char *line_buf){
	int num_word;
	int j;
	
	num_word = 0;
	if(line_buf[0] != ' ') num_word = 1;
	for (j = 1; j < nchara_l-1; j++) {
		if(line_buf[j-1] == ' ' && line_buf[j] != ' ') num_word = num_word + 1;
	};
	if(nchara_l == 0) num_word = -1;
	
	return num_word;
}


static void get_one_line_by_zlib(void *FP_z, int *num_buffer, int *num_word,
								 int *nchara, char *line_buf){
	*nchara = 0;
	gzgets(((gzFile) FP_z), line_buf, *num_buffer);
	
	*nchara = count_linechara(*num_buffer, line_buf);
	*num_word = count_words(*nchara, line_buf);
	
	/*
	fprintf(stderr,"num_buffer: %d, nchar_line %d, num_word %d\n",
			*num_buffer, *nchara, *num_word);
	*/
	
	if(*num_word == -1){
		fprintf(stderr, "increase text buffer size!!\n");
		fprintf(stderr, "%s \n",line_buf);
	}
	return;
}

void gzseek_go_fwd_c(int *ioffset, int *ierr){
    z_off_t ierr_z;
    ierr_z = gzseek(file_gzz, (z_off_t) *ioffset, SEEK_CUR);
    *ierr =  (int)ierr_z;
}

void gzread_32bit_c(const int *iflag_swap, int *ilength, char *textbuf, int *ierr){
    *ierr =  gzread(file_gzz, textbuf, (uInt) *ilength);
    *ierr = *ierr - *ilength;
    if(*iflag_swap == IFLAG_SWAP) {byte_swap_4(*ilength, textbuf);};
    return;
}

void gzread_64bit_c(const int *iflag_swap, int *ilength, char *textbuf, int *ierr){
    *ierr =  gzread(file_gzz, textbuf, (uInt) *ilength);
    *ierr = *ierr - *ilength;
    if(*iflag_swap == IFLAG_SWAP) {byte_swap_8(*ilength, textbuf);};
    return;
}

void gzwrite_c(int *ilength, void *buf, int *ierr){
    *ierr = gzwrite(file_gzz, buf, (uInt) *ilength);
    *ierr = *ierr - *ilength;
    return;
}

void get_one_line_from_gz_c(void *FP_z, int *num_buffer, 
							int *num_word, int *nchara, char *line_buf){
	get_one_line_by_zlib(FP_z, num_buffer, num_word, nchara, line_buf);
	line_buf[*nchara-1] = ' ';
	line_buf[*nchara  ] = '\n';
	return;
}

int skip_comment_gz_c(void *FP_z, int *num_buffer, char *buf){
	int nchara = 0, num_word = 0, icou = 0;
	
    get_one_line_from_gz_c(FP_z, num_buffer, &num_word, &nchara, buf);
	while ((nchara <= 1) || (buf[0] == '!') || (buf[0] == '#') || (buf[0] == '\n')) {
        get_one_line_from_gz_c(FP_z, num_buffer, &num_word, &nchara, buf);
		icou = icou + 1;
	};
	return num_word;
};

/* compress*/
void compress_file_c(const char *txt_file_name, const char *gz_file_name)
{
	z_stream z;                     /* data strucure for zlib */
	unsigned char inbuf[INBUFSIZ];           /* input buffer */
	unsigned char outbuf[OUTBUFSIZ];         /* output buffer */
	
	FILE *fin, *fout;               /* input and output file name */
	
	int count, flush, status;
	
	if ((fout = fopen(gz_file_name, "w")) == NULL) {
		fprintf(stderr, "Can't open %s\n", txt_file_name);
		exit(1);
	}
	if ((fin = fopen(txt_file_name, "r")) == NULL) {
		fprintf(stderr, "Can't open %s\n", gz_file_name);
		exit(1);
	}
	
	/* memory manegement is controlled by zlib */
	z.zalloc = Z_NULL;
	z.zfree =  Z_NULL;
	z.opaque = Z_NULL;

	/* Initialization */
	/* Second valuable: compression level between 0 and 9. 0 is non compress. */
	/* Default is Z_DEFAULT_COMPRESSION (= 6) */
	/*if (deflateInit(&z, Z_DEFAULT_COMPRESSION) != Z_OK) {
	fprintf(stderr, "deflateInit: %s\n", (z.msg) ? z.msg : "???");
	exit(1);
	}*/
	/*  Construct gzip compatible header */
	if (deflateInit2(&z, Z_DEFAULT_COMPRESSION, Z_DEFLATED, (Z_DEFAULT_COMPRESSION+32),
					Z_DEFAULT_MEMLEVEL, Z_DEFAULT_STRATEGY) != Z_OK) {
		fprintf(stderr, "deflateInit2: %s\n", (z.msg) ? z.msg : "???");
		exit(1);
	}
	
	z.avail_in = 0;             /* nuber of byte in input buffer */
	z.next_out = outbuf;        /* set output pointer */
	z.avail_out = OUTBUFSIZ;    /* set size of output buffer */
	
	/* Normally, the second valuable of deflate() is set to Z_NO_FLUSH */
	flush = Z_NO_FLUSH;
	
	while (1) {
		if (z.avail_in == 0) {  /* if input is finished */
			z.next_in = inbuf;  /* set input pointer to topof buffer */
			z.avail_in = (uint) fread(inbuf, 1, INBUFSIZ, fin); /* read data */
			/*printf("%s \n",inbuf);*/
			
			/* if input is end, set second valuable for  deflate() to Z_FINISH */
			if (z.avail_in < INBUFSIZ) flush = Z_FINISH;
		}
		status = deflate(&z, flush); /* compress */
		if (status == Z_STREAM_END) break; /* stop */
		if (status != Z_OK) {   /* error */
			fprintf(stderr, "deflate: %s\n", (z.msg) ? z.msg : "???");
			exit(1);
		}
		
		/* if output buffer is full, write data */
		if (z.avail_out == 0) {
			if (fwrite(outbuf, 1, OUTBUFSIZ, fout) != OUTBUFSIZ) {
				fprintf(stderr, "Write error\n");
				exit(1);
			}
			 /* return output buffer to initial */
			z.next_out = outbuf;
			z.avail_out = OUTBUFSIZ;
		}
	}
	
	/* output rest data */
	if ((count = OUTBUFSIZ - z.avail_out) != 0) {
		if (fwrite(outbuf, 1, count, fout) != count) {
			fprintf(stderr, "Write error\n");
			exit(1);
		}
	}
	
	/*  Finalize */
	if (deflateEnd(&z) != Z_OK) {
		fprintf(stderr, "deflateEnd: %s\n", (z.msg) ? z.msg : "???");
		exit(1);
	}
	
	fclose(fin);
	fclose(fout);
	
	if(remove(txt_file_name) == 0){
		printf(" %s is deleted.\n", txt_file_name);
	} else {
		printf(" Delete error \n");
		exit(1);
	};
	return;
}


void decompress_file_c(const char *gz_file_name, const char *txt_file_name) /* decompress data */
{
	z_stream z;                     /* data strucure for zlib */
	unsigned char inbuf[INBUFSIZ];           /* input buffer */
	unsigned char outbuf[OUTBUFSIZ];         /* output buffer */
	
	FILE *fin, *fout;               /* input and output file name */
	
	int count, status;
	
	if ((fin = fopen(gz_file_name, "r")) == NULL) {
		fprintf(stderr, "Can't open %s\n", gz_file_name);
		exit(1);
	}
	if ((fout = fopen(txt_file_name, "w")) == NULL) {
		fprintf(stderr, "Can't open %s\n", txt_file_name);
		exit(1);
	}
	
	/* memory manegement is controlled by zlib */
	z.zalloc = Z_NULL;
	z.zfree = Z_NULL;
	z.opaque = Z_NULL;
	
	/* Initialize */
	z.next_in = Z_NULL;
	z.avail_in = 0;
	/*    if (inflateInit(&z) != Z_OK) {
	fprintf(stderr, "inflateInit: %s\n", (z.msg) ? z.msg : "???");
	exit(1);
	}*/
	/*  read gzip compatible header */
	if (inflateInit2(&z, (Z_DEFAULT_COMPRESSION+32)) != Z_OK) {
		fprintf(stderr, "inflateInit: %s\n", (z.msg) ? z.msg : "???");
		exit(1);
	}
	
	z.next_out = outbuf;        /* set output pointer */
	z.avail_out = OUTBUFSIZ;    /* amount of output buffer */
	status = Z_OK;
	
	while (status != Z_STREAM_END) {
		if (z.avail_in == 0) {  /* If input buffer is full */
			z.next_in = inbuf;  /* return input pointer to initial */
			z.avail_in = (uint) fread(inbuf, 1, INBUFSIZ, fin); /* read gzipped file */
		}
		status = inflate(&z, Z_NO_FLUSH); /* extract */
		if (status == Z_STREAM_END) break; /* finish */
		if (status != Z_OK) {   /* error */
			fprintf(stderr, "inflate: %s\n", (z.msg) ? z.msg : "???");
			exit(1);
		}
		if (z.avail_out == 0) { /* output data when output buffer is full */
			if (fwrite(outbuf, 1, OUTBUFSIZ, fout) != OUTBUFSIZ) {
				fprintf(stderr, "Write error\n");
				exit(1);
			}
			z.next_out = outbuf; /* return output pointer */
			z.avail_out = OUTBUFSIZ; /* return output buffer size */
		}
	}
	
	/* output rest */
	if ((count = OUTBUFSIZ - z.avail_out) != 0) {
		if (fwrite(outbuf, 1, count, fout) != count) {
			fprintf(stderr, "Write error\n");
			exit(1);
		}
	}
	
	/* Finalize */
	if (inflateEnd(&z) != Z_OK) {
		fprintf(stderr, "inflateEnd: %s\n", (z.msg) ? z.msg : "???");
		exit(1);
	}
	
	fclose(fin);
	fclose(fout);
	
	if(remove(gz_file_name) == 0){
		printf(" %s is deleted.\n", gz_file_name);
	} else {
		printf(" Delete error \n");
		exit(1);
	};
	return;
}
