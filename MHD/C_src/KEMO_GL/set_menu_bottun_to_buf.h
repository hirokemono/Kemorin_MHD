
/* set_menu_bottun_to_buf.h */

#ifndef SET_MENU_BOTTUN_TO_BUF_
#define SET_MENU_BOTTUN_TO_BUF_

#include <math.h>
#include "ysglfontdata.h"
#include "vartex_array_object_gl.h"

#define MENU_HEIGHT 32
#define MENU_WIDTH  64

/* prototypes */

void menubottun_bitmap(GLubyte *menu_bitmap);

void const_menu_bottun_buffer(struct gl_strided_buffer *strided_buf);

#endif

