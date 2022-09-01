/*********************************************************************
    calypso_zlib_access_c.h
    fortran wrapper for zlib data compression and extraction
*********************************************************************/

#ifndef KEMO_ZLIB_ACCESS_C__
#define KEMO_ZLIB_ACCESS_C__

#include <stdio.h>
#include <stdlib.h>

#ifndef DEPENDENCY_CHECK
  #include <zlib.h>               /* /usr(/local)/include/zlib.h */
#endif

#include "calypso_param_c.h"
/* #include "numbers_to_bin_c.h" */

#define windowBits 15
#define GZIP_ENCODING 16
#define GZIP_AUTODETECT 16

/* prototypes */
void zlib_defleat_once_c(const int *len_buf, const void *buf,
						 const int *len_gzipbuf, int *len_gzipped, char *gzipbuf);
void gzip_defleat_once_c(const int *len_buf, const void *buf,
						 const int *len_gzipbuf, int *len_gzipped, char *gzipbuf);
void * zlib_defleat_begin_c(const int *len_buf, const void *buf, 
							const int *len_gzipbuf, int *len_gzipped, char *gzipbuf);
void * gzip_defleat_begin_c(const int *len_buf, const void *buf,
							const int *len_gzipbuf, int *len_gzipped, char *gzipbuf);
void gzip_defleat_cont_c(void *stream_gzip, const int *len_buf, 
						 const void *buf, const int *len_gzipbuf, int *len_gzipped);
void gzip_defleat_last_c(void *stream_gzip, const int *len_buf, 
						 const void *buf, const int *len_gzipbuf, int *len_gzipped);

void calypso_zlib_infleat_once(const int *len_gzipbuf, const char *gzipbuf,
							   const int *len_buf, void *buf, int *len_gzipped);

void calypso_gzip_infleat_once(const int *len_gzipbuf, const char *gzipbuf,
							   const int *len_buf, void *buf, int *len_gzipped);
void * calypso_zlib_infleat_begin(const int *len_gzipbuf, const char *gzipbuf,
								  const int *len_buf, void *buf, int *len_gzipped);
void * calypso_gzip_infleat_begin(const int *len_gzipbuf, const char *gzipbuf, 
								  const int *len_buf, void *buf, int *len_gzipped);
void calypso_gzip_infleat_cont(void *stream_gzip, const int *len_gzipbuf, 
							   const int *len_buf, void *buf, int *len_gzipped);
void calypso_gzip_infleat_last(void *stream_gzip, const int *len_gzipbuf, 
							   const int *len_buf, void *buf, int *len_gzipped);
#endif
