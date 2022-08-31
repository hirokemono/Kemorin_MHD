/*********************************************************************
 calypso_rawfile_io_c.h
    fortran wrapper for binary IO
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

void open_wt_rawfile(const char *file_name, int *ierr);
void open_ad_rawfile(const char *file_name, int *ierr);
void open_rd_rawfile(const char *file_name, int *ierr);
void close_rawfile(void);

void rawseek_go_fwd(int *ioffset, int *ierr);
void rawread_32bit(int *iflag_swap, int *ilength, void *buf, int *lenchara);
void rawread_64bit(int *iflag_swap, int *ilength, void *buf, int *lenchara);
void rawwrite(int *ilength, void *buf, int *lenchara);

#endif
