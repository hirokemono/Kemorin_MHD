/*********************************************************************
    calypso_zlib_io_old.h
    fortran wrapper for zlib IO
*********************************************************************/

#ifndef KEMO_ZLIB_IO_OLD__
#define KEMO_ZLIB_IO_OLD__

#include <stdio.h>
#include <stdlib.h>

#ifndef DEPENDENCY_CHECK
  #include <zlib.h>               /* /usr(/local)/include/zlib.h */
#endif

#include "calypso_param_c.h"
#include "numbers_to_bin_c.h"
#include "calypso_zlib_io_c.h"


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

void open_wt_gzfile(const char *gz_file_name);
void open_ad_gzfile(const char *gz_file_name);
void open_rd_gzfile(const char *gz_file_name);
void close_gzfile(void);

int open_rd_gzfile_w_flag(const char *gz_file_name);
int rewind_gzfile(void);
int check_gzfile_eof(void);

void write_compress_txt_nolf(int *nchara, char *input_txt);

void gzseek_go_fwd_f(int *ioffset, int *ierr);
void gzread_32bit_f(const int *iflag_swap, int *ilength, char *textbuf, int *ierr);
void gzread_64bit_f(const int *iflag_swap, int *ilength, char *textbuf, int *ierr);
void gzwrite_f(int *ilength, void *buf, int *ierr);

void get_one_line_from_gz(int *num_buffer, int *num_word, int *nchara, char *line_buf);
int skip_comment_gz(int *num_buffer, char *buf);

void zlib_defleat_once(const int *len_buf, const void *buf, const int *len_gzipbuf, 
                       int *len_gzipped, char *gzipbuf);
void zlib_defleat_begin(const int *len_buf, const void *buf, const int *len_gzipbuf, 
                        int *len_gzipped, char *gzipbuf);
void gzip_defleat_once(const int *len_buf, const void *buf, const int *len_gzipbuf, 
                       int *len_gzipped, char *gzipbuf);
void gzip_defleat_begin(const int *len_buf, const void *buf, const int *len_gzipbuf, 
                        int *len_gzipped, char *gzipbuf);
void gzip_defleat_cont(const int *len_buf, const void *buf, const int *len_gzipbuf, 
                       int *len_gzipped);
void gzip_defleat_last(const int *len_buf, const void *buf, const int *len_gzipbuf, 
                       int *len_gzipped);


void compress_file(const char *txt_file_name, const char *gz_file_name);
void decompress_file(const char *gz_file_name, const char *txt_file_name);
#endif
