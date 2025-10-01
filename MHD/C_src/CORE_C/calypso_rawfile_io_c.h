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

/* prototypes */

void open_wt_rawfile(const char *file_name, int *ierr);
void open_ad_rawfile(const char *file_name, int *ierr);
void open_rd_rawfile(const char *file_name, int *ierr);
void close_rawfile(void);

int rawseek_go_fwd(const long ioffset);
long rawread_32bit(const int iflag_swap, const long ilength, void *buf);
long rawread_64bit(const int iflag_swap, const long ilength, void *buf);
long rawwrite(const long ilength, void *buf);

#endif
