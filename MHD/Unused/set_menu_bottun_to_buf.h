
/* set_menu_bottun_to_buf.h */

#ifndef SET_MENU_BOTTUN_TO_BUF_
#define SET_MENU_BOTTUN_TO_BUF_

#include <math.h>

#include "m_vertex_buffer.h"
#include "ysglfontdata.h"

#define MENU_HEIGHT 32
#define MENU_WIDTH  64

/* prototypes */

void menubottun_bitmap(unsigned char *menu_bitmap);

void const_menu_bottun_buffer(struct gl_strided_buffer *strided_buf);

#endif

