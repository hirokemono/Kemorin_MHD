/*********************************************************************
    calypso_zlib_file_io_c.c
    fortran wrapper for zlib IO
*********************************************************************/

#include <string.h>
#include "calypso_zlib_file_io_c.h"

/* The following macro calls a zlib routine and checks the return
 value. If the return value ("status") is not OK, it prints an error
 message and exits the program. Zlib's error statuses are all less
 than zero. */

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

/* decompress file */
void decompress_file_c(const char *gz_file_name, const char *txt_file_name) 
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
