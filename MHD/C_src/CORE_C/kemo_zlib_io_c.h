/*********************************************************************
    kemo_zlib_io_c.h
    fortran wrapper for zlib IO
*********************************************************************/

#ifndef KEMO_ZLIB_IO_C__
#define KEMO_ZLIB_IO_C__

#include <stdio.h>
#include <stdlib.h>

#include "zlib.h"               /* /usr(/local)/include/zlib.h */

#include "kemosrc_param_c.h"


#define Z_DEFAULT_MEMLEVEL  8
#define GZ_WT_MODE "wb6f"
#define GZ_AD_MODE "ab6f"
#define GZ_RD_MODE "rb6f"

#define windowBits 15
#define GZIP_ENCODING 16

/* prototypes */

void open_wt_gzfile(const char *gz_file_name);
void open_ad_gzfile(const char *gz_file_name);
void open_rd_gzfile(const char *gz_file_name);
void close_gzfile();

int open_rd_gzfile_w_flag(const char *gz_file_name);
int check_gzfile_eof();

void write_compress_txt(int *num_buffer, char *input_txt);
void write_compress_txt_nolf(int *num_buffer, char *input_txt);
void get_one_line_from_gz(int *num_buffer, int *num_word, int *nchara, char *line_buf);
int skip_comment_gz_c(int *num_buffer, char *buf);

void gzip_defleat_once(int *len_buf, const char *buf, int *len_gzipbuf, 
                       int *len_gzipped, char *gzipbuf);
void gzip_defleat_begin(int *len_buf, const char *buf, int *len_gzipbuf, 
                        int *len_gzipped, char *gzipbuf);
void gzip_defleat_cont(int *len_buf, const char *buf, int *len_gzipbuf, int *len_gzipped);
void gzip_defleat_last(int *len_buf, const char *buf, int *len_gzipbuf, int *len_gzipped);


void compress_file(const char *txt_file_name, const char *gz_file_name);
void decompress_file(const char *gz_file_name, const char *txt_file_name);
#endif
