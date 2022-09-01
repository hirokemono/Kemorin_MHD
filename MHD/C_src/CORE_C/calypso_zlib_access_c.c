/*
********************************************************************
    calypso_zlib_access_c.c
    fortran wrapper for zlib data compression and extraction
********************************************************************
*/

#include <string.h>
#include "calypso_zlib_access_c.h"

/* The following macro calls a zlib routine and checks the return
 value. If the return value ("status") is not OK, it prints an error
 message and exits the program. Zlib's error statuses are all less
 than zero. */

#define CALL_ZLIB(x) {                       \
int status;                                  \
status = x;                                  \
if (status < 0) {                            \
fprintf (stderr,                             \
"%s:%d: %s returned a bad status of %d.\n",  \
__FILE__, __LINE__, #x, status);             \
exit (EXIT_FAILURE);                         \
    }                                        \
}

static z_stream * malloc_z_stream(){
	z_stream *stream_p = (z_stream *) malloc(sizeof(z_stream *));
	if(stream_p == NULL){
		printf("malloc error for z_stream\n");
		exit(1);
	};
	
    stream_p->zalloc = Z_NULL;
    stream_p->zfree  = Z_NULL;
    stream_p->opaque = Z_NULL;
	
	return stream_p;
}


static z_stream * init_zlib_deflate_stream()
{
	z_stream *stream_p = malloc_z_stream();
    CALL_ZLIB (deflateInit (stream_p, Z_DEFAULT_COMPRESSION));
    return stream_p;
}
static z_stream * init_zlib_inflate_stream()
{
	z_stream *stream_p = malloc_z_stream();
    CALL_ZLIB (inflateInit (stream_p));
    return stream_p;
}

static z_stream * init_gzip_deflate_stream_init()
{
	z_stream *stream_p = malloc_z_stream();
    CALL_ZLIB (deflateInit2 (stream_p, Z_DEFAULT_COMPRESSION, Z_DEFLATED,
                             windowBits | GZIP_ENCODING, 8,
                             Z_DEFAULT_STRATEGY));
    return stream_p;
}
static z_stream * init_gzip_inflate_stream()
{
	z_stream *stream_p = malloc_z_stream();
    CALL_ZLIB (inflateInit2 (stream_p, windowBits|GZIP_ENCODING|GZIP_AUTODETECT));
    return stream_p;
}


void zlib_defleat_once_c(const int *len_buf, const void *buf,
						 const int *len_gzipbuf, int *len_gzipped, char *gzipbuf)
{
	z_stream *stream_p = init_zlib_deflate_stream();
	
    stream_p->next_in = (unsigned char *) buf;
    stream_p->avail_in =  (uInt) *len_buf;
    stream_p->avail_out = (uInt) *len_gzipbuf;
    stream_p->next_out = (unsigned char *) gzipbuf;
    CALL_ZLIB (deflate (stream_p, Z_FINISH));
    *len_gzipped = *len_gzipbuf - stream_p->avail_out;
/*    printf("compressed size:%d %d %d \n",*len_buf, *len_gzipbuf, *len_gzipped);*/
    deflateEnd (stream_p);
	free(stream_p);
    return;
}

void gzip_defleat_once_c(const int *len_buf, const void *buf,
						 const int *len_gzipbuf, int *len_gzipped, char *gzipbuf)
{
	z_stream *stream_p = init_gzip_deflate_stream_init();
    stream_p->next_in = (unsigned char *) buf;
    stream_p->avail_in =  (uInt) *len_buf;
    stream_p->avail_out = (uInt) *len_gzipbuf;
	stream_p->next_out = (unsigned char *) gzipbuf;
	
    CALL_ZLIB (deflate (stream_p, Z_FINISH));
    *len_gzipped = *len_gzipbuf - stream_p->avail_out;
/*    printf("compressed size:%d %d %d \n",*len_buf, *len_gzipbuf, *len_gzipped);*/
	deflateEnd(stream_p);
	free(stream_p);
    return;
}

void * zlib_defleat_begin_c(const int *len_buf, const void *buf, 
							const int *len_gzipbuf, int *len_gzipped, char *gzipbuf)
{
	z_stream *stream_p = init_zlib_deflate_stream();
	
	stream_p->next_in = (unsigned char *) buf;
	stream_p->avail_in =  (uInt) *len_buf;
	stream_p->avail_out = (uInt) *len_gzipbuf;
	stream_p->next_out = (unsigned char *) gzipbuf;
	CALL_ZLIB (deflate (stream_p, Z_NO_FLUSH));
	*len_gzipped = *len_gzipbuf - stream_p->avail_out;
	
	return (void *) stream_p;
}

void * gzip_defleat_begin_c(const int *len_buf, const void *buf,
							const int *len_gzipbuf, int *len_gzipped, char *gzipbuf)
{
	z_stream *stream_p = init_gzip_deflate_stream_init();
    stream_p->next_in = (unsigned char *) buf;
    stream_p->avail_in =  (uInt) *len_buf;
    stream_p->avail_out = (uInt) *len_gzipbuf;
	stream_p->next_out = (unsigned char *) gzipbuf;
	
    CALL_ZLIB (deflate (stream_p, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - stream_p->avail_out;
	return (void *) stream_p;
}

void gzip_defleat_cont_c(void *stream_gzip, const int *len_buf, 
						 const void *buf, const int *len_gzipbuf, int *len_gzipped)
{
	z_stream *stream_p = (z_stream *) stream_gzip;
/*    uInt avail_out_current = stream_p->avail_out; */
    
    stream_p->next_in = (unsigned char *) buf;
    stream_p->avail_in =  (uInt) *len_buf;
    CALL_ZLIB (deflate (stream_p, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - stream_p->avail_out;
    return;
}

void gzip_defleat_last_c(void *stream_gzip, const int *len_buf, 
						 const void *buf, const int *len_gzipbuf, int *len_gzipped)
{
	z_stream *stream_p = (z_stream *) stream_gzip;
/*    uInt avail_out_current = stream_p->avail_out; */
    
    stream_p->next_in = (unsigned char *) buf;
    stream_p->avail_in =  (uInt) *len_buf;
    CALL_ZLIB (deflate (stream_p, Z_FINISH));
    *len_gzipped = *len_gzipbuf - stream_p->avail_out;
/*    printf("compressed size:%d %d %d \n",*len_buf, avail_out_current, *len_gzipped);*/
	deflateEnd (stream_p);
	free(stream_p);
    return;
}

void calypso_zlib_infleat_once(const int *len_gzipbuf, const char *gzipbuf,
							   const int *len_buf, void *buf, int *len_gzipped)
{
/*    printf("pointer:%p %p %d \n", gzipbuf, buf, *len_gzipped); */
	z_stream *stream_p = init_zlib_inflate_stream();
	
    stream_p->next_in = (unsigned char *) gzipbuf;
    stream_p->avail_in =  (uInt) *len_gzipbuf;
    stream_p->avail_out = (uInt) *len_buf;
    stream_p->next_out = (unsigned char *) buf;
    CALL_ZLIB (inflate (stream_p, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - stream_p->avail_in;
/*    printf("compressed size:%d %d %d \n",*len_buf, *len_gzipbuf, *len_gzipped); */
    inflateEnd (stream_p);
	free(stream_p);
    return;
}

void calypso_gzip_infleat_once(const int *len_gzipbuf, const char *gzipbuf,
							   const int *len_buf, void *buf, int *len_gzipped)
{
	z_stream *stream_p = init_gzip_inflate_stream();
	
    stream_p->next_in = (unsigned char *) gzipbuf;
    stream_p->avail_in =  (uInt) *len_gzipbuf;
    stream_p->avail_out = (uInt) *len_buf;
    stream_p->next_out = (unsigned char *) buf;
    CALL_ZLIB (inflate (stream_p, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - stream_p->avail_in;
/*    printf("compressed size:%d %d %d \n",*len_buf, *len_gzipbuf, *len_gzipped); */
    inflateEnd (stream_p);
	free(stream_p);
    return;
}

void * calypso_zlib_infleat_begin(const int *len_gzipbuf, const char *gzipbuf,
								  const int *len_buf, void *buf, int *len_gzipped)
{
	z_stream *stream_p = init_zlib_inflate_stream();
	
    stream_p->next_in = (unsigned char *) gzipbuf;
    stream_p->avail_in =  (uInt) *len_gzipbuf;
    stream_p->avail_out = (uInt) *len_buf;
    stream_p->next_out = (unsigned char *) buf;
    CALL_ZLIB (inflate (stream_p, Z_NO_FLUSH));
	*len_gzipped = *len_gzipbuf - stream_p->avail_in;
	
	return (void *) stream_p;
}

void * calypso_gzip_infleat_begin(const int *len_gzipbuf, const char *gzipbuf, 
								  const int *len_buf, void *buf, int *len_gzipped)
{
	z_stream *stream_p = init_gzip_inflate_stream();
	
    stream_p->next_in = (unsigned char *) gzipbuf;
    stream_p->avail_in =  (uInt) *len_gzipbuf;
    stream_p->avail_out = (uInt) *len_buf;
    stream_p->next_out = (unsigned char *) buf;
    CALL_ZLIB (inflate (stream_p, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - stream_p->avail_in;
	
	return (void *) stream_p;
}

void calypso_gzip_infleat_cont(void *stream_gzip, const int *len_gzipbuf, 
							   const int *len_buf, void *buf, int *len_gzipped)
{
	z_stream *stream_p = (z_stream *) stream_gzip;
    /* uInt avail_in_current = stream_p->avail_in; */
    
    stream_p->next_out = (unsigned char *) buf;
    stream_p->avail_out =  (uInt) *len_buf;
    CALL_ZLIB (inflate (stream_p, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - stream_p->avail_in;
    /*    printf("compressed size:%d %d %d \n",*len_buf, avail_in_current, *len_gzipped);*/
    return;
}

void calypso_gzip_infleat_last(void *stream_gzip, const int *len_gzipbuf, 
							   const int *len_buf, void *buf, int *len_gzipped)
{
	z_stream *stream_p = (z_stream *) stream_gzip;
    /* uInt avail_in_current = stream_p->avail_in; */
    
    stream_p->next_out = (unsigned char *) buf;
    stream_p->avail_out =  (uInt) *len_buf;
    CALL_ZLIB (inflate (stream_p, Z_NO_FLUSH));
    *len_gzipped = *len_gzipbuf - stream_p->avail_in;
    /*    printf("compressed size:%d %d %d \n",*len_buf, avail_in_current, *len_gzipped);*/
    inflateEnd (stream_p);
	free(stream_p);
    return;
}
