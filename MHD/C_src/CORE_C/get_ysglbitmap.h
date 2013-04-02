
/* get_ysglbitmap.h */

#ifndef GET_YSGLBITMAP_
#define GET_YSGLBITMAP_

#include "kemosrc_param_c.h"
#include "numbers_to_bin_c.h"
#include "ysglfontdata.h"

#define XSGL_8   8
#define YSGL_12 12


/* prototypes */

void generate_ysfont8x12(const char input, int i_font[12][8]);
void generate_ysfont8x12_c(const char input, int i_font[12][8]);

#endif
