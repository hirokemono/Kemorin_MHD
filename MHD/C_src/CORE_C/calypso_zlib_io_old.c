/*********************************************************************
    calypso_zlib_io_c.c
    fortran wrapper for zlib IO
*********************************************************************/

#include <string.h>
#include "calypso_zlib_io_old.h"

FILE *fp_z;
gzFile file_gz;

z_stream strm_gl;

/* The following macro calls a zlib routine and checks the return
 value. If the return value ("status") is not OK, it prints an error
 message and exits the program. Zlib's error statuses are all less
 than zero. */

#define CALL_ZLIB(x) {                                                  \
int status;                                                     \
status = x;                                                     \
if (status < 0) {                                               \
fprintf (stderr,                                            \
"%s:%d: %s returned a bad status of %d.\n",        \
__FILE__, __LINE__, #x, status);                   \
exit (EXIT_FAILURE);                                        \
    }                                                               \
}

void open_wt_gzfile(const char *gz_file_name){
    printf("Initial %p\n", &file_gz);
	file_gz = gzopen(gz_file_name, GZ_WT_MODE);
	if (file_gz == NULL){
		fprintf(stderr, "failed to gzopen\n");
		exit(1);
	}
//    open_wt_gzfile_c(gz_file_name, (void *) &file_gz);
    printf("Set %p\n", &file_gz);
    return;
}

void open_ad_gzfile(const char *gz_file_name){
	file_gz = gzopen(gz_file_name, GZ_AD_MODE);
	if (file_gz == NULL){
		fprintf(stderr, "failed to gzopen\n");
		exit(1);
	}
	return;
}
void open_rd_gzfile(const char *gz_file_name){
	file_gz = gzopen(gz_file_name, GZ_RD_MODE);
	if (file_gz == NULL){
		fprintf(stderr, "failed to gzopen\n");
		exit(1);
	}
	return;
}

void close_gzfile(void){
	int iret;
	
	if ((iret = gzclose(file_gz)) != Z_OK){
		fprintf(stderr, "gzclose failed.\n");
		exit(1);
	}
	
	return;
}

int open_rd_gzfile_w_flag(const char *gz_file_name){
	file_gz = gzopen(gz_file_name, GZ_RD_MODE);
	if (file_gz == NULL){
		fprintf(stderr, "failed to gzopen\n");
		return 1;
	}
	return 0;
}

int rewind_gzfile(void){
    return gzrewind(file_gz);
}

int check_gzfile_eof(void){
    return gzeof(file_gz);
}

void write_compress_txt_nolf(int *nchara, char *input_txt){
	int writelen, num_txt;
	
	num_txt = (int) strlen(input_txt);
	input_txt[num_txt] = '\0';
	writelen = gzwrite(file_gz, input_txt, num_txt);
	if (writelen != num_txt) {
		fprintf(stderr, "failed to gzwrite\n");
		exit(1);
	}
	
	memset(input_txt, '\0', sizeof(*nchara));
	return;
}

static void zlib_deflate_stream_init (z_stream *strm)
{
    strm->zalloc = Z_NULL;
    strm->zfree  = Z_NULL;
    strm->opaque = Z_NULL;
    CALL_ZLIB (deflateInit (strm, Z_DEFAULT_COMPRESSION));
    return;
}
static void zlib_inflate_stream_init (z_stream *strm)
{
    strm->zalloc = Z_NULL;
    strm->zfree  = Z_NULL;
    strm->opaque = Z_NULL;
    CALL_ZLIB (inflateInit (strm));
    return;
}

static void gzip_deflate_stream_init (z_stream *strm)
{
    strm->zalloc = Z_NULL;
    strm->zfree  = Z_NULL;
    strm->opaque = Z_NULL;
    CALL_ZLIB (deflateInit2 (strm, Z_DEFAULT_COMPRESSION, Z_DEFLATED,
                             windowBits | GZIP_ENCODING, 8,
                             Z_DEFAULT_STRATEGY));
    return;
}
static void gzip_inflate_stream_init (z_stream *strm)
{
    strm->zalloc = Z_NULL;
    strm->zfree  = Z_NULL;
    strm->opaque = Z_NULL;
    CALL_ZLIB (inflateInit2 (strm, windowBits|GZIP_ENCODING|GZIP_AUTODETECT));
    return;
}

void zlib_defleat_once(const int *len_buf, const void *buf, const int *len_gzipbuf, 
                       int *len_gzipped, char *gzipbuf)
{
    z_stream strm;

    zlib_deflate_stream_init (& strm);
    strm.next_in = (unsigned char *) buf;
    strm.avail_in =  (uInt) *len_buf;
    strm.avail_out = (uInt) *len_gzipbuf;
    strm.next_out = (unsigned char *) gzipbuf;
    CALL_ZLIB (deflate (& strm, Z_FINISH));
    *len_gzipped = *len_gzipbuf - strm.avail_out;
/*    printf("compressed size:%d %d %d \n",*len_buf, *len_gzipbuf, *len_gzipped);*/
    deflateEnd (& strm);
    return;
}

void zlib_defleat_begin(const int *len_buf, const void *buf, const int *len_gzipbuf, 
                        int *len_gzipped, char *gzipbuf)
{
    
    gzip_deflate_stream_init (& strm_gl);
    strm_gl.next_in = (unsigned char *) buf;
    strm_gl.avail_in =  (uInt) *len_buf;
    strm_gl.avail_out = (uInt) *len_gzipbuf;
    strm_gl.next_out = (unsigned char *) gzipbuf;
    CALL_ZLIB (deflate (& strm_gl, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - strm_gl.avail_out;
    return;
}

void gzip_defleat_once(const int *len_buf, const void *buf, const int *len_gzipbuf, 
                       int *len_gzipped, char *gzipbuf)
{
    z_stream strm;

    gzip_deflate_stream_init (& strm);
    strm.next_in = (unsigned char *) buf;
    strm.avail_in =  (uInt) *len_buf;
    strm.avail_out = (uInt) *len_gzipbuf;
    strm.next_out = (unsigned char *) gzipbuf;
    CALL_ZLIB (deflate (& strm, Z_FINISH));
    *len_gzipped = *len_gzipbuf - strm.avail_out;
/*    printf("compressed size:%d %d %d \n",*len_buf, *len_gzipbuf, *len_gzipped);*/
    deflateEnd (& strm);
    return;
}

void gzip_defleat_begin(const int *len_buf, const void *buf, const int *len_gzipbuf, 
                        int *len_gzipped, char *gzipbuf)
{
    
    gzip_deflate_stream_init (& strm_gl);
    strm_gl.next_in = (unsigned char *) buf;
    strm_gl.avail_in =  (uInt) *len_buf;
    strm_gl.avail_out = (uInt) *len_gzipbuf;
    strm_gl.next_out = (unsigned char *) gzipbuf;
    CALL_ZLIB (deflate (& strm_gl, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - strm_gl.avail_out;
    return;
}

void gzip_defleat_cont(const int *len_buf, const void *buf, const int *len_gzipbuf,
                       int *len_gzipped)
{
    uInt avail_out_current;
    
    avail_out_current = strm_gl.avail_out;
    
    strm_gl.next_in = (unsigned char *) buf;
    strm_gl.avail_in =  (uInt) *len_buf;
    CALL_ZLIB (deflate (& strm_gl, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - strm_gl.avail_out;
    return;
}

void gzip_defleat_last(const int *len_buf, const void *buf, const int *len_gzipbuf,
                       int *len_gzipped)
{
    uInt avail_out_current;
    
    avail_out_current = strm_gl.avail_out;
    
    strm_gl.next_in = (unsigned char *) buf;
    strm_gl.avail_in =  (uInt) *len_buf;
    CALL_ZLIB (deflate (& strm_gl, Z_FINISH));
    *len_gzipped = *len_gzipbuf - strm_gl.avail_out;
/*    printf("compressed size:%d %d %d \n",*len_buf, avail_out_current, *len_gzipped);*/
    deflateEnd (& strm_gl);
    return;
}

void zlib_infleat_once(const int *len_gzipbuf, const char *gzipbuf, const int *len_buf, 
                       void *buf, int *len_gzipped)
{
    z_stream strm;
    
/*    printf("pointer:%p %p %d \n", gzipbuf, buf, *len_gzipped); */
    
    zlib_inflate_stream_init (& strm);
    strm.next_in = (unsigned char *) gzipbuf;
    strm.avail_in =  (uInt) *len_gzipbuf;
    strm.avail_out = (uInt) *len_buf;
    strm.next_out = (unsigned char *) buf;
    CALL_ZLIB (inflate (& strm, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - strm.avail_in;
/*    printf("compressed size:%d %d %d \n",*len_buf, *len_gzipbuf, *len_gzipped); */
    inflateEnd (& strm);
    return;
}

void zlib_infleat_begin(const int *len_gzipbuf, const char *gzipbuf, const int *len_buf, 
                        void *buf, int *len_gzipped)
{
    
    zlib_inflate_stream_init (& strm_gl);
    strm_gl.next_in = (unsigned char *) gzipbuf;
    strm_gl.avail_in =  (uInt) *len_gzipbuf;
    strm_gl.avail_out = (uInt) *len_buf;
    strm_gl.next_out = (unsigned char *) buf;
    CALL_ZLIB (inflate (& strm_gl, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - strm_gl.avail_in;
    return;
}

void gzip_infleat_once(const int *len_gzipbuf, const char *gzipbuf, const int *len_buf, 
                       void *buf, int *len_gzipped)
{
    z_stream strm;
    
    gzip_inflate_stream_init (& strm);
    strm.next_in = (unsigned char *) gzipbuf;
    strm.avail_in =  (uInt) *len_gzipbuf;
    strm.avail_out = (uInt) *len_buf;
    strm.next_out = (unsigned char *) buf;
    CALL_ZLIB (inflate (& strm, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - strm.avail_in;
/*    printf("compressed size:%d %d %d \n",*len_buf, *len_gzipbuf, *len_gzipped); */
    inflateEnd (& strm);
    return;
}

void gzip_infleat_begin(const int *len_gzipbuf, const char *gzipbuf, const int *len_buf, 
                        void *buf, int *len_gzipped)
{
    
    gzip_inflate_stream_init (& strm_gl);
    strm_gl.next_in = (unsigned char *) gzipbuf;
    strm_gl.avail_in =  (uInt) *len_gzipbuf;
    strm_gl.avail_out = (uInt) *len_buf;
    strm_gl.next_out = (unsigned char *) buf;
    CALL_ZLIB (inflate (& strm_gl, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - strm_gl.avail_in;
    return;
}

void gzip_infleat_cont(const int *len_gzipbuf, const int *len_buf, 
                       void *buf, int *len_gzipped)
{
    uInt avail_in_current;
    
    avail_in_current = strm_gl.avail_in;
    
    strm_gl.next_out = (unsigned char *) buf;
    strm_gl.avail_out =  (uInt) *len_buf;
    CALL_ZLIB (inflate (& strm_gl, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - strm_gl.avail_in;
    /*    printf("compressed size:%d %d %d \n",*len_buf, avail_in_current, *len_gzipped);*/
    return;
}

void gzip_infleat_last(const int *len_gzipbuf, const int *len_buf,
                       void *buf, int *len_gzipped)
{
    uInt avail_in_current;
    
    avail_in_current = strm_gl.avail_in;
    
    strm_gl.next_out = (unsigned char *) buf;
    strm_gl.avail_out =  (uInt) *len_buf;
    CALL_ZLIB (inflate (& strm_gl, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - strm_gl.avail_in;
    /*    printf("compressed size:%d %d %d \n",*len_buf, avail_in_current, *len_gzipped);*/
    inflateEnd (& strm_gl);
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


static void get_one_line_by_zlib(int *num_buffer, int *num_word, int *nchara, char *line_buf){
	*nchara = 0;
	gzgets(file_gz, line_buf, *num_buffer);
	
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

void gzseek_go_fwd_f(int *ioffset, int *ierr){
    z_off_t ierr_z;
    ierr_z = gzseek(file_gz, (z_off_t) *ioffset, SEEK_CUR);
    *ierr =  (int)ierr_z;
}

void gzread_32bit_f(const int *iflag_swap, int *ilength, char *textbuf, int *ierr){
    *ierr =  gzread(file_gz, textbuf, (uInt) *ilength);
    *ierr = *ierr - *ilength;
    if(*iflag_swap == IFLAG_SWAP) {byte_swap_4(*ilength, textbuf);};
    return;
}

void gzread_64bit_f(const int *iflag_swap, int *ilength, char *textbuf, int *ierr){
    *ierr =  gzread(file_gz, textbuf, (uInt) *ilength);
    *ierr = *ierr - *ilength;
    if(*iflag_swap == IFLAG_SWAP) {byte_swap_8(*ilength, textbuf);};
    return;
}

void gzwrite_f(int *ilength, void *buf, int *ierr){
    *ierr = gzwrite(file_gz, buf, (uInt) *ilength);
    *ierr = *ierr - *ilength;
    return;
}

void get_one_line_from_gz(int *num_buffer, int *num_word, int *nchara, char *line_buf){
	
	get_one_line_by_zlib(num_buffer, num_word, nchara, line_buf);
	line_buf[*nchara-1] = ' ';
	line_buf[*nchara  ] = '\n';
	return;
}

int skip_comment_gz(int *num_buffer, char *buf){
	int nchara = 0, num_word = 0, icou = 0;
	
	get_one_line_from_gz(num_buffer, &num_word, &nchara, buf);
	while ((nchara <= 1) || (buf[0] == '!') || (buf[0] == '#') || (buf[0] == '\n')) {
		get_one_line_from_gz(num_buffer, &num_word, &nchara, buf);
		icou = icou + 1;
	};
	return num_word;
};

/* compress*/
void compress_file(const char *txt_file_name, const char *gz_file_name)
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


void decompress_file(const char *gz_file_name, const char *txt_file_name) /* decompress data */
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
