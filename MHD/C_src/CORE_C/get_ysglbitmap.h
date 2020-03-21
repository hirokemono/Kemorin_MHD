
/* get_ysglbitmap.h */

#ifndef GET_YSGLBITMAP_
#define GET_YSGLBITMAP_

#include "calypso_param_c.h"
#include "numbers_to_bin_c.h"
#include "ysglfontdata.h"

#define XSGL_8   8
#define YSGL_12 12
#define XSGL_16 16
#define YSGL_24 24


/* prototypes */

void generate_ysfont8x12(const char input, int i_font[12][8]);
void generate_ysfont16x24(const char input, int i_font[24][16]);

void generate_ysfont8x12_c(const char input, int i_font[12][8]);
void generate_ysfont16x24_c(const char input, int i_font[24][16]);

#endif
