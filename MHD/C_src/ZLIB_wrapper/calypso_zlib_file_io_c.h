/*********************************************************************
    calypso_zlib_file_io_c.h
    fortran wrapper for zlib IO
*********************************************************************/

#ifndef KEMO_ZLIB_FILE_IO_C__
#define KEMO_ZLIB_FILE_IO_C__

#include <stdio.h>
#include <stdlib.h>

#ifndef DEPENDENCY_CHECK
  #include <zlib.h>               /* /usr(/local)/include/zlib.h */
#endif

#include "calypso_param_c.h"
#include "numbers_to_bin_c.h"

#define Z_DEFAULT_MEMLEVEL  8

/* prototypes */
void compress_file_c(const char *txt_file_name, const char *gz_file_name);
void decompress_file_c(const char *gz_file_name, const char *txt_file_name);
#endif
