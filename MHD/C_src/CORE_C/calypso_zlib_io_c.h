/*********************************************************************
    calypso_zlib_io_c.h
    fortran wrapper for zlib IO
*********************************************************************/

#ifndef KEMO_ZLIB_IO_C__
#define KEMO_ZLIB_IO_C__

#include <stdio.h>
#include <stdlib.h>

#ifndef DEPENDENCY_CHECK
  #include <zlib.h>               /* /usr(/local)/include/zlib.h */
#endif

#include "calypso_param_c.h"
#include "numbers_to_bin_c.h"


#define Z_DEFAULT_MEMLEVEL  8
#define GZ_WT_MODE "wb6f"
#define GZ_AD_MODE "ab6f"
#define GZ_RD_MODE "rb6f"

#define RAW_WT_MODE "wb0"
#define RAW_AD_MODE "ab0"
#define RAW_RD_MODE "rb0"

#define windowBits 15
#define GZIP_ENCODING 16
#define GZIP_AUTODETECT 16

/* prototypes */

void* open_wt_gzfile_c(const char *gz_file_name);
void* open_ad_gzfile_c(const char *gz_file_name);
void* open_rd_gzfile_c(const char *gz_file_name);
void close_gzfile_c(void *FP_z);

int open_rd_gzfile_w_flag_c(const char *gz_file_name);
int rewind_gzfile_c(void);
int check_gzfile_eof_c(void);

void write_compress_txt_c(void *FP_z, int *nchara, char *input_txt);
void write_compress_txt_nolf_c(int *nchara, char *input_txt);

void gzseek_go_fwd_c(int *ioffset, int *ierr);
void gzread_32bit_c(const int *iflag_swap, int *ilength, char *textbuf, int *ierr);
void gzread_64bit_c(const int *iflag_swap, int *ilength, char *textbuf, int *ierr);
void gzwrite_c(int *ilength, void *buf, int *ierr);

void zlib_defleat_once_c(const int *len_buf, const void *buf, const int *len_gzipbuf,
                       int *len_gzipped, char *gzipbuf);
void zlib_defleat_begin_c(const int *len_buf, const void *buf, const int *len_gzipbuf,
                        int *len_gzipped, char *gzipbuf);
void gzip_defleat_once_c(const int *len_buf, const void *buf, const int *len_gzipbuf,
                       int *len_gzipped, char *gzipbuf);
void gzip_defleat_begin_c(const int *len_buf, const void *buf, const int *len_gzipbuf,
                        int *len_gzipped, char *gzipbuf);
void gzip_defleat_cont_c(const int *len_buf, const void *buf, const int *len_gzipbuf,
                       int *len_gzipped);
void gzip_defleat_last_c(const int *len_buf, const void *buf, const int *len_gzipbuf,
                       int *len_gzipped);

void zlib_infleat_once_c(const int *len_gzipbuf, const char *gzipbuf, const int *len_buf,
                       void *buf, int *len_gzipped);
void zlib_infleat_begin_c(const int *len_gzipbuf, const char *gzipbuf, const int *len_buf,
                        void *buf, int *len_gzipped);
void gzip_infleat_once_c(const int *len_gzipbuf, const char *gzipbuf, const int *len_buf,
                       void *buf, int *len_gzipped);
void gzip_infleat_begin_c(const int *len_gzipbuf, const char *gzipbuf, const int *len_buf,
                        void *buf, int *len_gzipped);
void gzip_infleat_cont_c(const int *len_gzipbuf, const int *len_buf,
                       void *buf, int *len_gzipped);
void gzip_infleat_last_c(const int *len_gzipbuf, const int *len_buf, 
                       void *buf, int *len_gzipped);

void get_one_line_from_gz_c(void *FP_z, int *num_buffer, int *num_word,
							int *nchara, char *line_buf);
int skip_comment_gz_c(void *FP_z, int *num_buffer, char *buf);


void compress_file_c(const char *txt_file_name, const char *gz_file_name);
void decompress_file_c(const char *gz_file_name, const char *txt_file_name);
#endif
